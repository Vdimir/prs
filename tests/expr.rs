
// #![feature(box_patterns)]

extern crate prs;

use prs::pars::{Token, Parse, predicate, fn_parser};
use prs::stream::char_stream::CharStream;
use prs::stream::vec_stream::VecStream;
use prs::comb::{ParserComb,  ParserCombDynamic};
use prs::comb::{eof, many, maybe};
use prs::result::ParseErr;
use prs::stream::TokenStream;

#[derive(PartialEq, Debug, Clone)]
enum ExprToken {
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
    fn num(self) -> u32 {
        match self {
            ExprToken::Num(n) => Ok(n),
            _ => Err("not a number"),
        }
        .unwrap()
    }

    fn iden(self) -> String {
        match self {
            ExprToken::Iden(s) => Ok(s),
            _ => Err("not a identifier"),
        }
        .unwrap()
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
    let stream = &mut CharStream::new(input);

    let op_symb = Token('+').then(|_| ExprToken::Add)
                  .or(Token('-').then(|_| ExprToken::Sub)
                  .or(Token('*').then(|_| ExprToken::Mul)))
                  .or(Token('/').then(|_| ExprToken::Div));

    let paren = Token('(').then(|_| ExprToken::LParen)
                .or(Token(')').then(|_| ExprToken::RParen));

    let digit = predicate(|c: &char| c.is_digit(10));

    let num = digit.many()
                .then(|s: String| ExprToken::Num(s.parse::<u32>().unwrap()));



    fn alphabetic(c: char) -> Option<char> {
        if c.is_alphabetic() { Some(c) } else { None }
    }

    fn alphanumeric(c: char) -> Option<char> {
        if c.is_alphanumeric() { Some(c) } else { None }
    }

    fn iden_f(input: &mut CharStream) -> Result<ExprToken, ParseErr<char>> {
        if let Some(_) = input.peek().and_then(|c| alphabetic(c)) {
            let mut res = String::new();
            while let Some(alph) = input.peek().and_then(|c| alphanumeric(c)) {
                res.push(alph);
                input.next();
            }
            Ok(ExprToken::Iden(res))
        } else {
            Err(ParseErr::unexpected(input.peek()))
        }

    }
    let iden = fn_parser(iden_f);

    //let ws = Token(' ');

    let lexem = op_symb
                .or(num)
                .or(iden)
                .or(paren);

    let lexer = many(
        lexem.skip_any(Token(' '))
        )
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

type BoxPair<T> = Box<(T, T)>;

#[derive(PartialEq, Debug)]
enum Node {
    Num(u32),
    Iden(String),
    Add(BoxPair<Node>),
    Sub(BoxPair<Node>),
    Mul(BoxPair<Node>),
    Div(BoxPair<Node>),
}

use std::collections::HashMap;
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

    fn calc(&self, vars: &HashMap<String, u32>) -> u32 {
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

fn parse_expr(s: &str) -> Result<Node, ParseErr<ExprToken>> {
    let tokens: &mut VecStream<ExprToken> = &mut VecStream::new(tokenize(s).unwrap());

    fn list_to_tree((mut node, rest): (Node, Option<Vec<(ExprToken, Node)>>)) -> Node {
        for (op, rh) in rest .unwrap_or(vec![]) .into_iter() {
            node = Node::create_op(op, node, rh);
        }
        node
    }

    fn expression(s: &mut VecStream<ExprToken>) -> Result<Node, ParseErr<ExprToken>>
    {
        let num = predicate(|t: &ExprToken| t.is_num())
                    .then(|t: ExprToken| Node::Num(t.num()) );

        let iden = predicate(|t: &ExprToken| t.is_iden())
                    .then(|t: ExprToken| Node::Iden(t.iden()) );

        let parens_exp =
            (Token(ExprToken::LParen),
            fn_parser(expression),
            Token(ExprToken::RParen))
            .then(|(_, e, _)| e);

        let factor = num.or(iden).or(parens_exp);
        let op_symb = predicate(|t: &ExprToken| t.is_mul_op());

        let inf_mul = fn_parser(move |s| {
            (&factor, maybe(many((&op_symb, &factor))))
                .then(list_to_tree)
                .parse(s)
        });

        let add_symb = predicate(|t: &ExprToken| t.is_add_op());
        let expr_parser  = (&inf_mul, (add_symb, &inf_mul)
                                               .many().maybe())
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

    let ast = parse_expr("5+ eight /4+3*(9+4-2*3)-((2+3))-8+(2+3)").unwrap();
    assert_eq!(ast.calc(&variables), 20);

    assert_eq!(parse_expr("(3").ok(), None);
    assert_eq!(parse_expr("1+5 9").ok(), None);
    assert_eq!(parse_expr("+").ok(), None);
    assert_eq!(parse_expr("5+*3").ok(), None);
    assert_eq!(parse_expr("-9").ok(), None);
}

