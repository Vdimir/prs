
extern crate prs;

use prs::pars::{Token, Parse, predicate, fn_parser};
use prs::stream::char_stream::CharStream;
use prs::stream::vec_stream::VecStream;
use prs::comb::ParserComb;
use prs::comb::{eof, many, maybe, wrap, many0, Seq};
use prs::result::ParseErr;

use std::collections::HashMap;

#[derive(PartialEq, Debug, Clone)]
enum ExprToken {
    Num(i32),
    Iden(String),
    Add,
    Sub,
    Mul,
    Div,
    LParen,
    RParen,
}

impl ExprToken {
    fn num(self) -> i32 {
        match self {
            ExprToken::Num(n) => n,
            _ => panic!("not a number"),
        }
    }

    fn iden(self) -> String {
        match self {
            ExprToken::Iden(s) => s,
            _ => panic!("not a identifier"),
        }
    }

    fn is_num(&self) -> bool {
        match *self {
            ExprToken::Num(_) => true,
            _ => false,
        }
    }

    fn is_iden(&self) -> bool {
        match *self {
            ExprToken::Iden(_) => true,
            _ => false,
        }
    }

    fn is_add_op(&self) -> bool {
        match *self {
            ExprToken::Add | ExprToken::Sub => true,
            _ => false,
        }
    }

    fn is_mul_op(&self) -> bool {
        match *self {
            ExprToken::Mul | ExprToken::Div => true,
            _ => false,
        }
    }
}

fn tokenize(input: &str) -> Result<Vec<ExprToken>, ParseErr<char>> {
    use ExprToken::*;
    let stream = &mut CharStream::new(input);

    let plus    = Token('+').then(|_| Add);
    let minus   = Token('-').then(|_| Sub);
    let mul     = Token('*').then(|_| Mul);
    let div     = Token('/').then(|_| Div);
    let l_paren = Token('(').then(|_| LParen);
    let r_paren = Token(')').then(|_| RParen);

    let op_symb = wrap(plus.or(minus).or(mul).or(div));
    let paren = l_paren.or(r_paren);
    let digit = wrap(predicate(|c: &char| c.is_digit(10)));

    let num = many(digit.clone())
                .then(|s: String| s.parse::<i32>().unwrap())
                .then(Num);

    let letter = predicate(|c| char::is_alphabetic(*c))
                        .then(|c: char| c.to_string());
    let letdig = predicate(|c| char::is_alphanumeric(*c));
    let iden = letter.and(many0(letdig))
                .then(|s| Iden(s));
    let lexem = wrap(op_symb.or(num).or(iden).or(paren));

    let lexer = many(lexem.skip_any(Token(' '))).skip(eof());
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
    Num(i32),
    Iden(String),
    Add(Box<(Node, Node)>),
    Sub(Box<(Node, Node)>),
    Mul(Box<(Node, Node)>),
    Div(Box<(Node, Node)>),
}

impl Node {
    fn calc(&self, vars: &HashMap<String, i32>) -> i32 {
        use Node::*;
        match *self {
            Num(n) => n,
            // with box_pattern:
            // Add(box (ref a, ref b)) => a.calc() + b.calc(),
            Add(ref pair) => pair.0.calc(vars) + pair.1.calc(vars),
            Sub(ref pair) => pair.0.calc(vars) - pair.1.calc(vars),
            Mul(ref pair) => pair.0.calc(vars) * pair.1.calc(vars),
            Div(ref pair) => pair.0.calc(vars) / pair.1.calc(vars),
            Iden(ref s) => *vars.get(s).unwrap(),
        }
    }
}

fn create_op(op: ExprToken, lhs: Node, rhs: Node) -> Node {
    match op {
        ExprToken::Add => Node::Add(Box::new((lhs, rhs))),
        ExprToken::Sub => Node::Sub(Box::new((lhs, rhs))),
        ExprToken::Mul => Node::Mul(Box::new((lhs, rhs))),
        ExprToken::Div => Node::Div(Box::new((lhs, rhs))),
        _ => panic!("{:?} not allowed", op),
    }
}

fn parse_expr(s: &str) -> Result<Node, ParseErr<ExprToken>> {
    let tokens: &mut VecStream<ExprToken> = &mut VecStream::new(tokenize(s).unwrap());

    fn list_to_tree((mut node, rest): (Node, Vec<(ExprToken, Node)>)) -> Node {
        for (op, rh) in rest {
            node = create_op(op, node, rh);
        }
        node
    }

    fn expression(s: &mut VecStream<ExprToken>) -> Result<Node, ParseErr<ExprToken>>
    {
        let num = predicate(ExprToken::is_num)
                    .then(|t: ExprToken| t.num())
                    .then(Node::Num);

        let iden = predicate(ExprToken::is_iden)
                    .then(|t: ExprToken| Node::Iden(t.iden()));

        let parens_exp = wrap(
            (Token(ExprToken::LParen),
            fn_parser(expression),
            Token(ExprToken::RParen))
            .then(|(_, e, _)| e));

        let factor = wrap(num.or(iden).or(parens_exp));
        let op_symb = predicate(ExprToken::is_mul_op);

        let inf_mul = wrap((factor.clone(), many0((op_symb, factor)))
                .then(list_to_tree));

        let add_symb = predicate(ExprToken::is_add_op);
        let expr_parser = (inf_mul.clone(), many0((add_symb, inf_mul)))
                                .then(list_to_tree);
        expr_parser.parse(s)
    }

    fn_parser(expression)
    .skip(eof())
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

    let mut variables = HashMap::new();
    variables.insert("zero".to_owned(), 0);
    variables.insert("one".to_owned(), 1);
    variables.insert("eight".to_owned(), 8);

    let ast = parse_expr("5+ eight /4+3*(9+4-2*3)-((2+zero+3))-8+(2+3)+ zero").unwrap();
    assert_eq!(ast.calc(&variables), 20);

    assert_eq!(parse_expr("(3").ok(), None);
    assert_eq!(parse_expr("1+5 9").ok(), None);
    assert_eq!(parse_expr("+").ok(), None);
    assert_eq!(parse_expr("5+*3").ok(), None);
    assert_eq!(parse_expr("-9").ok(), None);
}

