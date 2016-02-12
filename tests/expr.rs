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
/*
use prs::stream::vec_stream::VecStream;
fn parse_expr(s: &str) -> Result<Node, ()> {

    let tokens = &mut VecStream::new(tokenize(s).unwrap());

    fn num_p<S>(s: &mut S) -> Result<Node, ()>
    where S: TokenStream<Token=ExprToken>,
    {
        if let Some(ExprToken::Num(n)) = s.peek() {
            s.next();
            Ok(Node::Num(n))
        } else {
            Err(())
        }
    }

    fn expression<S>(s: &mut S) -> Result<Node, ()>
    where S: SavableStream<Token=ExprToken>,
    {
        fn lust_to_tree((mut f, r): (Node, Option<Vec<(ExprToken, Node)>>)) -> Node {
            for (op, rh) in r.unwrap_or(Vec::new()).into_iter() {
                f = Node::create_op(op, f, rh);
            }
            f
        }

        let num = fn_parser(num_p);
        let parens_exp = 
            (Token(ExprToken::LParen).on_err(()),
            fn_parser(expression),
            Token(ExprToken::RParen).on_err(()))
            .then(|(_, e, _)| e);

        let factor = num.or(parens_exp).on_err(());

        let inf_mul = (&factor, maybe(many((
            predicate(|t: &ExprToken| t.is_mul_op()),
            &factor)
        )))
        .then(lust_to_tree);

        let inf_add = (&inf_mul, maybe(many((
            predicate(|t: &ExprToken| t.is_add_op()),
            &inf_mul)
        )))
        .then(lust_to_tree);

        inf_add.parse(s)
        // Err(())
    }

    fn_parser(expression).parse(tokens)
}

#[test]
fn expr_test() {


    assert_eq!(parse_expr("3"), Ok(Node::Num(3)));

    assert_eq!(parse_expr("1+2*3"), Ok(
        Node::Add(
            Box::new((
                Node::Num(1),
                Node::Mul(
                    Box::new((Node::Num(2),Node::Num(2))))
            )))));

    assert_eq!(parse_expr("(1+2)*3"), Ok(
        Node::Mul(Box::new(
            (Node::Add(Box::new(
                (Node::Num(1), Node::Num(2)))),
            Node::Num(3))
            ))));

}
*/
// ******************************************************************************

// #[test]
// fn expr_test() {

//     #[derive(PartialEq, Debug, Clone)]
//     enum Node {
//         Num(i32),
//         Add(Box<(Node, Node)>),
//         Sub(Box<(Node, Node)>),
//         Mul(Box<(Node, Node)>),
//         Div(Box<(Node, Node)>),
//     }

//     impl Node {
//         fn create_op(op_symb: char, lhs: Node, rhs: Node) -> Self {
//             match op_symb {
//                 '+' => Node::Add(Self::node_pair(lhs, rhs)),
//                 '-' => Node::Sub(Self::node_pair(lhs, rhs)),
//                 '*' => Node::Mul(Self::node_pair(lhs, rhs)),
//                 '/' => Node::Div(Self::node_pair(lhs, rhs)),
//                 _ => panic!("{:?} not allowed", op_symb),
//             }
//         }

//         fn node_pair(lhs: Node, rhs: Node) -> Box<(Node, Node)> {
//             Box::new((lhs, rhs))
//         }

//         fn calc(&self) -> i32 {
//             match self {
//                 &Node::Num(n) => n,
//                 &Node::Add(ref box_pair) => box_pair.0.calc() + box_pair.1.calc(),
//                 &Node::Sub(ref box_pair) => box_pair.0.calc() - box_pair.1.calc(),
//                 &Node::Mul(ref box_pair) => box_pair.0.calc() * box_pair.1.calc(),
//                 &Node::Div(ref box_pair) => box_pair.0.calc() / box_pair.1.calc(),
//             }
//         }
//     }

//     fn list_to_tree((mut f, r): (Node, Option<Vec<(char, Node)>>)) -> Node {
//         for &(op,  ref rh) in r.unwrap_or(Vec::new()).iter() {
//             f = Node::create_op(op, f, rh.clone());
//         }
//         return f;
//     }

//     fn num<'a>(tokens: CharsStream<'a>) -> ParseResult<Node, CharsStream<'a>> {
//         (pred(|c: &char| c.is_numeric())
//              .greedy()
//              .skip(maybe(token(' ').greedy()))
//              .map(|s: &str| Node::Num(s.parse::<i32>().unwrap())))
//             .parse(tokens)
//     }

//     fn parens_expr<'a>(tokens: CharsStream<'a>) -> ParseResult<Node, CharsStream<'a>> {
//         (token('(')
//              .skip(maybe(token(' ').greedy()))
//              .and(fn_parser(add_op).skip(token(')').skip(maybe(token(' ').greedy())))))
//             .map(|(_, a)| a)
//             .parse(tokens)
//     }

//     fn mul_op<'a>(tokens: CharsStream<'a>) -> ParseResult<Node, CharsStream<'a>> {
//         let mul_symb = token('*').skip(maybe(token(' ').greedy()));
//         let div_symb = token('/').skip(maybe(token(' ').greedy()));;
//         let mul_div = mul_symb.or(div_symb);
//         fn_parser(num)
//             .or(fn_parser(parens_expr))
//             .and(maybe(rep(mul_div.and(fn_parser(num).or(fn_parser(parens_expr))))))
//             .map(list_to_tree)
//             .parse(tokens)
//     }

//     assert_eq!(fn_parser(mul_op).parse(CharsStream::new("018 ")).res.unwrap(), Node::Num(18));

//     fn add_op<'a>(tokens: CharsStream<'a>) -> ParseResult<Node, CharsStream<'a>> {
//         let add_symb = token('+').skip(maybe(token(' ').greedy()));
//         let sub_symb = token('-').skip(maybe(token(' ').greedy()));;
//         let add_sub_symb = add_symb.or(sub_symb);
//         fn_parser(mul_op)
//             .and(maybe(rep(add_sub_symb.and(fn_parser(mul_op)))))
//             .map(list_to_tree)
//             .parse(tokens)
//     }

//     assert_eq!(fn_parser(mul_op).parse(CharsStream::new("18 /  9  * 3 *1")).res.unwrap().calc(),
//                6);

//     assert_eq!(fn_parser(add_op).parse(CharsStream::new("1+5 /  9  * 3")).res.unwrap(),
//                Node::Add(Box::new((Node::Num(1),
//                                    Node::Mul(Box::new((Node::Div(Box::new((Node::Num(5),
//                                                                            Node::Num(9)))),
//                                                        Node::Num(3))))))));

//     assert_eq!(fn_parser(add_op)
//                    .parse(CharsStream::new("5+ 16 /  (9 + 5 * (2 - (7))) + (40) /10 *2-6*9/3*(7-5)+6/2"))
//                    .res
//                    .unwrap()
//                    .calc(), -21);
// }


 