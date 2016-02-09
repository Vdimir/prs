#[cfg(test)]

extern crate prs;

use prs::pars::Parse;
use prs::pars::Token;
use prs::stream::char_stream::CharStream;
use prs::stream::TokenStream;

use prs::result::Expected;
use prs::comb::ParserComb;

#[test]
fn token_test() {
    let mut input = CharStream::new("xyz");

    assert_eq!(Token('x').parse(&mut input), Ok('x'));
    assert_eq!(Token('y').parse(&mut input), Ok('y'));
    assert_eq!(Token('w').parse(&mut input), Err(Expected('w')));
    assert_eq!(Token('z').parse(&mut input), Ok('z'));
}

use prs::pars::predicate;
#[test]
fn pred_test() {
    let mut input = CharStream::new("12a");

    // let dig = predicate::<_,CharStream,_>("digit",|c| c.is_digit(10));
    let dig = predicate("digit",|c: &char| c.is_digit(10));

    assert_eq!(dig.parse(&mut input), Ok('1'));
    assert_eq!(dig.parse(&mut input), Ok('2'));
    assert_eq!(dig.parse(&mut input), Err(Expected("digit".to_owned())));
    assert_eq!(input.peek(), Some('a'));
}


#[test]
fn or_test() {

    let mut input = CharStream::new("xyz");
    let x_or_y = Token('x').or(Token('y'));

    assert_eq!(x_or_y.parse(&mut input), Ok('x'));
    assert_eq!(x_or_y.parse(&mut input), Ok('y'));
    assert_eq!(x_or_y.parse(&mut input), Err((Expected('x'), Expected('y'))));

    let xyz = x_or_y.or(Token('z'));
    assert_eq!(xyz.parse(&mut input), Ok('z'));
}

#[test]
fn then_test() {
    let dig_square = predicate("digit",|c: &char| c.is_digit(10))
            .then(|c: char| c.to_digit(10).map(|d| d * d).unwrap());

    let input = &mut CharStream::new("123a");
    assert_eq!(dig_square.parse(input), Ok(1));
    assert_eq!(dig_square.parse(input), Ok(4));
    assert_eq!(dig_square.parse(input), Ok(9));
    assert_eq!(dig_square.parse(input), Err(Expected("digit".to_owned())));
}


use prs::comb::many;

#[test]
fn many_test() {
    let dig = many(predicate("digit",|c: &char| c.is_digit(10)));

    let input = &mut CharStream::new("123a");
    assert_eq!(dig.parse(input), Ok(vec!['1','2','3']));
    assert_eq!(dig.parse(input), Err(Expected("digit".to_owned())));
    assert_eq!(input.peek(), Some('a'));
}

#[test]
fn many_uni_test() {
    let dig = predicate("digit",|c: &char| c.is_digit(10))
                .many()
                .then(|s: String| s.parse::<u32>().unwrap());
                
    assert_eq!(dig.parse(&mut CharStream::new("123")), Ok(123));
}

// use prs::comb::OnError;
// #[test]
// fn onerr_test() {

//     let mut input = CharStream::new("xyz");

//     let x_pars = OnError(Token('x'), Expected("token `x`".to_owned()));
//     assert_eq!(x_pars.parse(&mut input), Ok('x'));
//     assert_eq!(x_pars.parse(&mut input), Err(Expected("token `x`".to_owned())));
// }



// ******************************************************************************


// #[test]
// fn mabye_test() {
//     let num_parser = pred(|c: &char| c.is_numeric())
//                          .greedy()
//                          .map(|s: &str| s.parse::<i32>().unwrap());

//     let mabye_num = maybe(num_parser);

//     let test_list = &[("123", Some(Some(123)), ""), ("A1", Some(None), "A1")];

//     for t in test_list {

//         let parsed = mabye_num.parse(CharsStream::new(t.0));
//         assert_eq!(parsed.res.ok(), t.1);
//         assert_eq!(parsed.other.rest(), t.2);
//     }
// }


// #[test]
// fn array_of_tokens_test() {
//     let tokens: &[&str] = &["655", "bar"];

//     let num_parser = named_pred("numb", |c: &&str| {
//         c.chars().all(|c| c.is_numeric())
//     });

//     assert_eq!(num_parser.parse(tokens).res,
//               Ok("655"));

//     let bar_parser = pred(|c: &&str| c == &"bar");

//     let tokens: &[&str] = &["bar", "655"];
//     assert_eq!(bar_parser.parse(tokens).res,
//               Ok("bar"));

//     assert_eq!(bar_parser.parse(&["bare"]).res.is_err(), true);
//     assert_eq!(bar_parser.parse(&["ba"]).res.is_err(), true);


//     // let tokens: &[&str] = &["655", "bar"];
//     // assert_eq!(num_parser.and(bar_parser).parse(tokens).res,
//     //           Ok(("655", "bar")));
    
// }

// #[test]
// fn parse_iter() {
//     let digit = pred(|c: &char| c.is_numeric());

//     let mut iter = iterate_parser_over_input(digit, CharsStream::new("123.45"));

//     assert_eq!(iter.next(), Some('1'));
//     assert_eq!(iter.next(), Some('2'));
//     assert_eq!(iter.next(), Some('3'));
//     assert_eq!(iter.next(), None);
//     assert_eq!(iter.input.rest(), ".45");
// }



// 

// #[test]
// fn skip_test() {
//     let num_parser = pred(|c: &char| c.is_numeric())
//                          .greedy()
//                          .map(|s: &str| s.parse::<i32>().unwrap());

//     let uppercase_parser = pred(|c: &char| c.is_uppercase()).greedy();

//     let space_parser = pred(|c: &char| c == &' ').greedy();

//     let num_space_uppercase = num_parser.skip(space_parser)
//                                         .and(uppercase_parser);

//     let test_list = &[ 
//                       ("5", None, "5"),
//                       ("123AB", None, "123AB"),
//                       ("123 AB", Some((123, "AB")), ""),
//                       ("123 ABcde", Some((123, "AB")), "cde")];

//     for t in test_list {

//         let parsed = num_space_uppercase.parse(CharsStream::new(t.0));
//         assert_eq!(parsed.res.ok(), t.1);
//         assert_eq!(parsed.other.rest(), t.2);
//     }
// }



// #[test]
// fn rep_test() {
//     let num_parser = pred(|c: &char| c.is_numeric())
//                          .greedy()
//                          .map(|s: &str| s.parse::<i32>().unwrap());

//     let space_parser = pred(|c: &char| c == &' ').greedy().map(|_| ());

//     let list_of_nums_sum = rep(num_parser.skip(maybe(space_parser)))
//                                .map(|x: Vec<_>| x.iter().fold(0, |acc, &x| acc + x));

//     let test_list = &[
//                       (" 1Aa", None, " 1Aa"),
//                       ("5", Some(5), ""),
//                       ("5 10 15 30 1 ", Some(61), ""),
//                       ("500 50  bar", Some(550), "bar")
//                       ];

//     for t in test_list {
//         let parsed = list_of_nums_sum.parse(CharsStream::new(t.0));
//         assert_eq!(parsed.res.ok(), t.1);
//         assert_eq!(parsed.other.rest(), t.2);
//     }
// }

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


