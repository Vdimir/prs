extern crate prs;

use prs::pars::Token;
use prs::stream::char_stream::CharStream;
use prs::comb::ParserComb;
use prs::comb::nop;
use prs::comb::eof;
use prs::comb::ParserCombDynamic;
use prs::pars::Parse;
use prs::pars::predicate;
use prs::comb::many;
// use prs::result::ParseErr;


#[derive(PartialEq, Debug)]
enum OpToken {
    Plus,
    Minus,
    Mul,
    Div
}
#[derive(PartialEq, Debug)]
enum ParenToken {
    LParen,
    RParen,
}
#[derive(PartialEq, Debug)]
enum ExprToken {
    None,
    Num(u32),
    Iden(String),
    Op(OpToken),
    Paren(ParenToken)
}

fn tokenize(input: &str) -> Result<Vec<ExprToken>, ()> {
    let stream = &mut CharStream::new(input);

    let op_symb = Token('+').then(|_| OpToken::Plus)
                  .or(Token('-').then(|_| OpToken::Minus)
                  .or(Token('*').then(|_| OpToken::Mul)))
                  .or(Token('/').then(|_| OpToken::Div))
                  .then(|t| ExprToken::Op(t));

    let paren = Token('(').then(|_| ParenToken::LParen)
        .or(Token(')').then(|_| ParenToken::RParen))
        .then(|t| ExprToken::Paren(t));

    let digit = predicate("digit",|c: &char| c.is_digit(10));

    let num = digit.many()
                .then(|s: String| ExprToken::Num(s.parse::<u32>().unwrap()));

    let letter = predicate("alphabetic",|c: &char| c.is_alphabetic());

    // let iden = (letter, many(letter.or(digit)))
                // .then(|(c, s): (char, Vec<char>)| ExprToken::Iden(
                    // Some(c).into_iter().chain(s.into_iter()).collect()));
    let iden = many(letter)
                .then(|s: String| ExprToken::Iden(s));

    let ws = Token(' ').then(|_| ExprToken::None);

    let lexer = //(nop().skip_any(&ws),
        many(op_symb
                .or(num)
                .or(iden)
                .or(paren)
                .skip_any(&ws))
        // ).then(|(_,x)| x)
        .on_err(())
        .skip(eof());
            
    return lexer.parse(stream);
}

#[test]
fn tokenize_test() {

    let res = tokenize("150  +( foo)  ");
    assert_eq!(res, Ok(vec![
            ExprToken::Num(150),
            ExprToken::Op(OpToken::Plus),
            ExprToken::Paren((ParenToken::LParen)),
            ExprToken::Iden("foo".to_owned()),
            ExprToken::Paren((ParenToken::RParen)),
        ]));

    let res = tokenize("5 + 4a");
    println!("{:?}", res);    
    panic!("TODO");
}



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


