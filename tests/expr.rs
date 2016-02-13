extern crate prs;

use prs::pars::Token;
use prs::stream::char_stream::CharStream;
use prs::stream::TokenStream;
use prs::stream::SavableStream;
use prs::comb::ParserComb;
use prs::comb::nop;
use prs::comb::eof;
use prs::comb::ParserCombDynamic;
use prs::pars::Parse;
use prs::pars::predicate;
use prs::pars::fn_parser;
use prs::comb::many;
use prs::comb::maybe;
// use prs::result::ParseErr;
use prs::comb::Pair;



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

    fn parse_op<S>(s: &mut S) -> Result<ExprToken, ()>
    where S: TokenStream<Token=char>,
    {

        let opt_res = s.peek().and_then(|c| 
            match c {
                '+' => Some(ExprToken::Add),
                '-' => Some(ExprToken::Sub),
                '*' => Some(ExprToken::Mul),
                '/' => Some(ExprToken::Div),
                _ => None,
            });

        opt_res
        .into_iter()
        .inspect(|_| {s.next();})
        .next()
        .ok_or(())
    }

    let op_symb = fn_parser(parse_op);

    // let op_symb = Token('+').then(|_| OpToken::Add)
    //               .or(Token('-').then(|_| OpToken::Sub)
    //               .or(Token('*').then(|_| OpToken::Mul)))
    //               .or(Token('/').then(|_| OpToken::Div))
    //               .then(|t| ExprToken::Op(t));

    let paren = Token('(').then(|_| ExprToken::LParen)
        .or(Token(')').then(|_| ExprToken::RParen));

    let digit = predicate(|c: &char| c.is_digit(10));

    let num = digit.many()
                .then(|s: String| ExprToken::Num(s.parse::<u32>().unwrap()));


    let alph = predicate(|c: &char| c.is_alphabetic());
    let alph_num = predicate(|c: &char| c.is_alphanumeric());

    // alloc overhead
    let iden = (alph, maybe(many(alph_num)))
                .then(|(c, v): (char, Option<Vec<_>>)| {
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

    let lexer = //(nop().skip_any(&ws),
        many(op_symb
                .or(num)
                .or(iden)
                .or(paren)
                .skip_any(&ws))
        .on_err(())
        .skip(eof());
            
    return lexer.parse(stream);
}

#[test]
fn tokenize_test() {

    assert_eq!(tokenize("150  +( foo12 ) * a "), Ok(vec![
            ExprToken::Num(150),
            ExprToken::Add,
            ExprToken::LParen,
            ExprToken::Iden("foo12".to_owned()),
            ExprToken::RParen,
            ExprToken::Mul,
            ExprToken::Iden("a".to_owned()),
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

}

use prs::stream::vec_stream::VecStream;
fn parse_expr(s: &str) -> Result<Node, ()> {

    let tokens = &mut VecStream::new(tokenize(s).unwrap());

    fn num_p(s: &mut VecStream<ExprToken>) -> Result<Node, ()>
    // where S: TokenStream<Token=ExprToken>,
    {
        if let Some(ExprToken::Num(n)) = s.peek() {
            s.next();
            Ok(Node::Num(n))
        } else {
            Err(())
        }
    }

    fn expression(s: &mut VecStream<ExprToken>) -> Result<Node, ()>
    // where S: SavableStream<Token=ExprToken>,
    {
        fn list_to_tree((mut f, r): (Node, Option<Vec<(ExprToken, Node)>>)) -> Node {
            for (op, rh) in r.unwrap_or(Vec::new()).into_iter() {
                f = Node::create_op(op, f, rh);
            }
            f
        }


        // let factor = ;

        fn fac(s: &mut VecStream<ExprToken>) -> Result<Node, ()> {

            let num = fn_parser(num_p);
            let parens_exp = 
                (Token(ExprToken::LParen).on_err(()),
                fn_parser(expression),
                Token(ExprToken::RParen).on_err(()))
                .then(|(_, e, _)| e);
            num.or(parens_exp).on_err(()).parse(s)
        }


        let inf_mul = (fn_parser(fac), maybe(many((
            predicate(|t: &ExprToken| t.is_mul_op()),
            fn_parser(fac))
        )))
        .then(list_to_tree);

        let inf_mul0= (fn_parser(fac), maybe(many((
            predicate(|t: &ExprToken| t.is_mul_op()),
            fn_parser(fac))
        )))
        .then(list_to_tree);

        let inf_add = Pair
        (Box::new(inf_mul), Box::new(maybe(many(
           Pair(Box::new(predicate(|t: &ExprToken| t.is_add_op())),
           Box::new(inf_mul0))
        ))))
        .then(list_to_tree);

        let expr_parser = inf_add;
        // let expr_parser = inf_mul;
        expr_parser.parse(s)
        // Err(())
    }

    (fn_parser(expression), eof())
                    .then(|(d,_)| d)
                    .parse(tokens)
}

#[test]
fn expr_test() {


    assert_eq!(parse_expr("3"), Ok(Node::Num(3)));

    assert_eq!(parse_expr("(1*2)*3"), Ok(
        Node::Mul(Box::new(
            (Node::Mul(Box::new(
                (Node::Num(1), Node::Num(2)))),
            Node::Num(3))
            ))));

    assert_eq!(parse_expr("1+2*3"), Ok(
        Node::Add(
            Box::new((
                Node::Num(1),
                Node::Mul(
                    Box::new((Node::Num(2),Node::Num(3))))
            )))));


    assert_eq!(parse_expr("1+5 /  9  * 3"), Ok(
               Node::Add(Box::new((Node::Num(1),
                       Node::Mul(Box::new((Node::Div(Box::new((Node::Num(5),
                                           Node::Num(9)))),
                       Node::Num(3)))))))));

}
