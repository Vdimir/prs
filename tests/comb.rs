
extern crate prs;
use prs::*;

// #[test]
// fn or_test() {
//     #[derive(Debug, PartialEq)]
//     enum NumOrString<'a> {
//         Num(i32),
//         Str(&'a str),
//         Space,
//     }

//     let num_parser = pred(|c: &char| c.is_numeric())
//                          .map(|s: &str| NumOrString::Num(s.parse::<i32>().unwrap()));

// let uppercase_parser = pred(|c: &char| c.is_uppercase()).map(|s| NumOrString::Str(s));

// let space_parser = pred(|c: &char| c == &' ').map(|_| NumOrString::Space);

//     let num_or_uppercase = num_parser.or(space_parser)
//                                      .or(uppercase_parser);


//     let test_list = &[("633XA", (Ok(NumOrString::Num(633)), "XA")),
//                       ("XA5", (Ok(NumOrString::Str("XA")), "5")),
//                       ("633", (Ok(NumOrString::Num(633)), "")),
//                       (" 633", (Ok(NumOrString::Space), "633")),
//                       ("   x ", (Ok(NumOrString::Space), "x ")),
//                       ("d5A", (Err(()), "d5A")),
//                       ("6A33xa", (Ok(NumOrString::Num(6)), "A33xa")),
//                       ("FOO", (Ok(NumOrString::Str("FOO")), ""))];

//     for t in test_list {
//         let res = num_or_uppercase.parse(t.0);
//         println!("{} {:?}", t.0, res);
//         assert_eq!(res, t.1);
//     }
// }

// #[test]
// fn and_test() {
//     let num_parser = pred(|c: &char| c.is_numeric()).map(|s: &str| (s.parse::<i32>().unwrap()));
//     {
//         let uppercase_parser = pred(|c: &char| c.is_uppercase());

// let num_or_uppercase = num_parser.and(uppercase_parser);

//         let test_list = &[("633XA", (Ok((633, "XA")), "")),
//                           ("5", (Err(()), "5")),
//                           ("633X", (Ok((633, "X")), "")),
//                           ("XA", (Err(()), "XA")),
//                           ("500FFbar", (Ok((500, "FF")), "bar")),
//                           ("d5A", (Err(()), "d5A"))];

//         for t in test_list {
//             let res = num_or_uppercase.parse(t.0);
//             println!("{} {:?}", t.0, res);
//             assert_eq!(res, t.1);
//         }
//     }
// }

// #[test]
// fn skip_test() {
//     let num_parser = pred(|c: &char| c.is_numeric()).map(|s: &str| s.parse::<i32>().unwrap());

// let uppercase_parser = pred(|c: &char| c.is_uppercase());

// let space_parser = pred(|c: &char| c == &' ').map(|_| ());

//     let num_space_uppercase = num_parser.skip(space_parser)
//                                         .and(uppercase_parser);

//     let test_list = &[("633 XA", (Ok((633, "XA")), "")),
//                       ("5", (Err(()), "5")),
//                       ("633X", (Err(()), "633X")),
//                       ("XA", (Err(()), "XA")),
//                       ("500 FFbar", (Ok((500, "FF")), "bar"))];

//     for t in test_list {
//         let res = num_space_uppercase.parse(t.0);
//         println!("{} {:?}", t.0, res);
//         assert_eq!(res, t.1);
//     }
// }


// #[test]
// fn mabye_test() {
//     let num_parser = pred(|c: &char| c.is_numeric()).map(|s: &str| s.parse::<i32>().unwrap());

// let mabye_num = maybe(num_parser);

// let test_list = &[("633x", (Ok(Some(633)), "x")), ("X5", (Ok(None), "X5"))];

//     for t in test_list {
//         let res = mabye_num.parse(t.0);
//         println!("{} {:?}", t.0, res);
//         assert_eq!(res, t.1);
//     }
// }

// #[test]
// fn rep_test() {
//     let num_parser = pred(|c: &char| c.is_numeric()).map(|s: &str| s.parse::<i32>().unwrap());

// let space_parser = pred(|c: &char| c == &' ').map(|_| ());

//     let list_of_nums_sum = rep(num_parser.skip(maybe(space_parser)))
//                                .map(|x| x.iter().fold(0, |acc, &x| acc + x));

//     let test_list = &[("5", (Ok(5), "")),
//                       ("1 2 3 4 50  12 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1",
//                        (Ok(100), "")),
//                       ("633X", (Ok(633), "X")),
//                       (" 5XA", (Err(()), " 5XA")),
//                       ("500 20  bar", (Ok(520), "bar"))];

//     for t in test_list {
//         let res = list_of_nums_sum.parse(t.0);
//         println!("{} {:?}", t.0, res);
//         assert_eq!(res, t.1);
//     }
// }

// #[test]
// fn num_parser_test() -> () {
//     let num_parser = pred(|c: &char| c.is_numeric());

// let num_parser_num = num_parser.map(|x: &str| x.parse::<i32>().unwrap());

//     let test_list = &[("633xa", (Ok(633), "xa")),
//                       ("-1", (Err(()), "-1")),
//                       ("633", (Ok(633), "")),
//                       ("a633xa", (Err(()), "a633xa")),
//                       ("6_33xa", (Ok(6), "_33xa")),
//                       ("s633a", (Err(()), "s633a"))];

//     for t in test_list {
//         let res = num_parser_num.parse(t.0);
//         println!("{} {:?}", t.0, res);
//         assert_eq!(res, t.1);
//     }
// }




// #[test]
// fn expr_test() {
//     enum Node {
//         Num(i32),
//         Add(Box<(Node, Node)>),
//         Sub(Box<(Node, Node)>),
//         Mul(Box<(Node, Node)>),
//         Div(Box<(Node, Node)>),
//     }

// let num = pred(|c: &char| c.is_numeric()).map(|s: &str| s.parse::<i32>().unwrap());

//     let add_symb = token('+');
//     let sub_symb = token('-');
//     let add_sub_symb = add_symb.or(sub_symb);
//     assert_eq!(add_sub_symb.parse("+-"), (Ok('+'), "-"));
// }

#[test]
fn simple_char_test() {
    let x_char = token('x');
    assert_eq!(x_char.parse("xxy").res, Ok('x'));
}
