
#![feature(box_patterns)]

extern crate prs;

use prs::pars::{Token, Parse, predicate, fn_parser};
use prs::stream::TokenStream;
use prs::stream::char_stream::CharStream;
use prs::stream::vec_stream::VecStream;
use prs::comb::{ParserComb,  ParserCombDynamic};
use prs::comb::{eof, many, maybe};


#[derive(PartialEq, Debug, Clone)]
enum ExprToken {
    None,
    Num(u32),
    Iden(String),
    Add,
    Sub,
    Mul,
    Div,
    LParen,
    RParen,
}

impl ExprToken {
    fn is_mul_op(&self) -> bool {
        match *self {
            ExprToken::Mul | ExprToken::Div => true,
            _ => false,
        }
    }

    fn is_add_op(&self) -> bool {
        match *self {
            ExprToken::Add | ExprToken::Sub => true,
            _ => false,
        }
    }
}

fn tokenize(input: &str) -> Result<Vec<ExprToken>, ()> {
    let stream = &mut CharStream::new(input);

    let op_symb = Token('+').then(|_| ExprToken::Add)
                  .or(Token('-').then(|_| ExprToken::Sub)
                  .or(Token('*').then(|_| ExprToken::Mul)))
                  .or(Token('/').then(|_| ExprToken::Div));

    // fn parse_op<S>(s: &mut S) -> Result<ExprToken, ()>
    // where S: TokenStream<Token=char>,
    // {
    //     let opt_res = s.peek().and_then(|c| 
    //         match c {
    //             '+' => Some(ExprToken::Add),
    //             '-' => Some(ExprToken::Sub),
    //             '*' => Some(ExprToken::Mul),
    //             '/' => Some(ExprToken::Div),
    //             _ => None,
    //         });
    //     opt_res
    //     .into_iter()
    //     .inspect(|_| {s.next();})
    //     .next()
    //     .ok_or(())
    // }

    // let op_symb = fn_parser(parse_op);

    let paren = Token('(').then(|_| ExprToken::LParen)
                .or(Token(')').then(|_| ExprToken::RParen));

    let digit = predicate(|c: &char| c.is_digit(10));

    let num = digit.many()
                .then(|s: String| ExprToken::Num(s.parse::<u32>().unwrap()));


    let alph = predicate(|c: &char| c.is_alphabetic());
    let alph_num = predicate(|c: &char| c.is_alphanumeric());

    let iden = (alph, maybe(many(alph_num)))
                .then(|(c, v): (char, Option<Vec<_>>)| {
                    // alloc overhead
                    ExprToken::Iden(
                        Some(c)
                        .into_iter()
                        .chain(
                            v.unwrap_or(Vec::new()).into_iter()
                        )
                        .collect()
                    )
                });

    let ws = Token(' ').then(|_| ExprToken::None);

    let lexer = many(op_symb
                .or(num)
                .or(iden)
                .or(paren)
                .skip_any(&ws))
        .supress_err()
        .skip(eof());
            
    return lexer.parse(stream);
}

#[test]
fn tokenize_test() {
    use ExprToken::*;

    assert_eq!(tokenize("150  +( foo12 ) * a "), Ok(vec![
            Num(150),
            Add,
            LParen,
            Iden("foo12".to_owned()),
            RParen,
            Mul,
            Iden("a".to_owned()),
        ]));

    assert!(tokenize("150 % 2").is_err());
}

#[derive(PartialEq, Debug)]
enum Node {
    Num(u32),
    Add(Box<(Node, Node)>),
    Sub(Box<(Node, Node)>),
    Mul(Box<(Node, Node)>),
    Div(Box<(Node, Node)>),
}

impl Node {
    fn create_op(op: ExprToken, lhs: Node, rhs: Node) -> Self {
        match op {
            ExprToken::Add => Node::Add(Box::new((lhs, rhs))),
            ExprToken::Sub => Node::Sub(Box::new((lhs, rhs))),
            ExprToken::Mul => Node::Mul(Box::new((lhs, rhs))),
            ExprToken::Div => Node::Div(Box::new((lhs, rhs))),
            _ => panic!("{:?} not allowed", op),
        }
    }

    fn calc(&self) -> u32 {
        use Node::*;

        match *self {
            Num(n) => n,
            Add(box (ref a, ref b)) => a.calc() + b.calc(),
            Sub(ref pair) => pair.0.calc() - pair.1.calc(),
            Mul(ref pair) => pair.0.calc() * pair.1.calc(),
            Div(ref pair) => pair.0.calc() / pair.1.calc(),
        }
    }
}

fn parse_expr(s: &str) -> Result<Node, ()> {

    let tokens = &mut VecStream::new(tokenize(s).unwrap());

    fn num_p(s: &mut VecStream<ExprToken>) -> Result<Node, ()>
    {
        if let Some(ExprToken::Num(n)) = s.peek() {
            s.next();
            Ok(Node::Num(n))
        } else {
            Err(())
        }
    }

    fn list_to_tree((mut f, r): (Node, Option<Vec<(ExprToken, Node)>>)) -> Node {
        for (op, rh) in r.unwrap_or(Vec::new()).into_iter() {
            f = Node::create_op(op, f, rh);
        }
        f
    }

    fn fac(s: &mut VecStream<ExprToken>) -> Result<Node, ()> {
        use ExprToken::*;
        
        let num = fn_parser(num_p);

        let parens_exp = 
            (Token(LParen).supress_err(),
            fn_parser(expression),
            Token(RParen).supress_err())
            .then(|(_, e, _)| e);

        num.or(parens_exp).supress_err().parse(s)
    }

    fn inf_mul_f(s: &mut VecStream<ExprToken>) -> Result<Node, ()> {
        let factor = fn_parser(fac);
        let op_symb = predicate(|t: &ExprToken| t.is_mul_op());

        (&factor, maybe(many((op_symb, &factor))))
        .then(list_to_tree)
        .parse(s)
    }

    fn expression(s: &mut VecStream<ExprToken>) -> Result<Node, ()>
    {
        let inf_mul = fn_parser(inf_mul_f);

        // let inf_add = Pair
        // (Box::new(&inf_mul),
        //     Box::new(maybe(many(
        //         Pair(Box::new(predicate(|t: &ExprToken| t.is_add_op())),
        //         Box::new(&inf_mul))
        // ))))
        // .then(list_to_tree);

        let add_symb = predicate(|t: &ExprToken| t.is_add_op());

        let expr_parser = (&inf_mul, maybe(many((add_symb, &inf_mul))))
                    .then(list_to_tree);

        expr_parser.parse(s)
    }

    fn_parser(expression).skip(eof())
    .parse(tokens)
}

#[test]
fn expr_test() {
    use Node::*;

    fn pair(a: Node, b: Node) -> Box<(Node, Node)> {
        Box::new((a, b))
    }

    assert_eq!(parse_expr("3"), Ok(Num(3)));

    assert_eq!(parse_expr("(1*2)*3"), Ok(
        Mul(pair(
            Mul(pair(Num(1),
                     Num(2))),
            Num(3)))));

    assert_eq!(parse_expr("1+2*3"), Ok(
        Add(pair(
                Num(1),
                Mul(pair(Num(2),
                         Num(3)))))));

    assert_eq!(parse_expr("1+5 /  9  * 3"), Ok(
               Add(pair(Num(1),
                        Mul(pair(
                            Div(pair(Num(5),
                                     Num(9))),
                            Num(3)))))));

    let ast = parse_expr("5+8/4+3*(9+4-2*3)-((2+3))-8+(2+3)").unwrap();
    assert_eq!(ast.calc(), 20);

    assert_eq!(parse_expr("(3").ok(), None);
    assert_eq!(parse_expr("1+5 9").ok(), None);
    assert_eq!(parse_expr("+").ok(), None);
    assert_eq!(parse_expr("5+*3").ok(), None);
    assert_eq!(parse_expr("-9").ok(), None);
}
