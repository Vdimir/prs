#[cfg(test)]

extern crate prs;
use prs::*;

#[test]
fn parse_iter() {
    let num_parser = pred(|c: &char| c.is_numeric());

    let mut iter = iterate_parser_over_input(num_parser, "598x65");

    assert_eq!(iter.next(), Some('5'));
    assert_eq!(iter.next(), Some('9'));
    assert_eq!(iter.next(), Some('8'));
    assert_eq!(iter.next(), None);
    assert_eq!(iter.next(), None);
    assert_eq!(iter.input, "x65");
}

#[test]
fn or_test() {
    #[derive(Debug, PartialEq)]
    enum NumOrString<'a> {
        Num(i32),
        Str(&'a str),
        Space,
    }

    let num_parser = pred(|c: &char| c.is_numeric())
                         .greedy()
                         .map(|s: &str| NumOrString::Num(s.parse::<i32>().unwrap()));

    let uppercase_parser = pred(|c: &char| c.is_uppercase()).greedy().map(|s| NumOrString::Str(s));

    let space_parser = pred(|c: &char| c == &' ').greedy().map(|_| NumOrString::Space);

    let num_or_uppercase = num_parser.or(space_parser)
                                     .or(uppercase_parser);


    let test_list = &[("633XA", Some(NumOrString::Num(633)), "XA"),
                      ("XA5", Some(NumOrString::Str("XA")), "5"),
                      ("633", Some(NumOrString::Num(633)), ""),
                      (" 633", Some(NumOrString::Space), "633"),
                      ("   x ", Some(NumOrString::Space), "x "),
                      ("d5A", None, "d5A"),
                      ("6A33xa", Some(NumOrString::Num(6)), "A33xa"),
                      ("FOO", Some(NumOrString::Str("FOO")), "")];


    for t in test_list {
        let parsed = num_or_uppercase.parse(t.0);
        assert_eq!(parsed.res.ok(), t.1);
        assert_eq!(parsed.other, t.2);
    }
}

#[test]
fn and_test() {
    let num_parser = pred(|c: &char| c.is_numeric())
                         .greedy()
                         .map(|s: &str| (s.parse::<i32>().unwrap()));

    let uppercase_parser = pred(|c: &char| c.is_uppercase()).greedy();

    let num_or_uppercase = num_parser.and(uppercase_parser);

    let test_list = &[("633XA", Some((633, "XA")), ""),
                      ("5", None, "5"),
                      ("633X", Some((633, "X")), ""),
                      ("XA", None, "XA"),
                      ("500FFbar", Some((500, "FF")), "bar"),
                      ("d5A", None, "d5A")];


    for t in test_list {
        let parsed = num_or_uppercase.parse(t.0);
        assert_eq!(parsed.res.ok(), t.1);
        assert_eq!(parsed.other, t.2);
    }

}

#[test]
fn skip_test() {
    let num_parser = pred(|c: &char| c.is_numeric())
                         .greedy()
                         .map(|s: &str| s.parse::<i32>().unwrap());

    let uppercase_parser = pred(|c: &char| c.is_uppercase()).greedy();

    let space_parser = pred(|c: &char| c == &' ').greedy();

    let num_space_uppercase = num_parser.skip(space_parser)
                                        .and(uppercase_parser);

    let test_list = &[("633 XA", Some((633, "XA")), ""),
                      ("5", None, "5"),
                      ("633X", None, "633X"),
                      ("XA", None, "XA"),
                      ("500 FFbar", Some((500, "FF")), "bar")];


    for t in test_list {
        let parsed = num_space_uppercase.parse(t.0);
        assert_eq!(parsed.res.ok(), t.1);
        assert_eq!(parsed.other, t.2);
    }
}


#[test]
fn mabye_test() {
    let num_parser = pred(|c: &char| c.is_numeric())
                         .greedy()
                         .map(|s: &str| s.parse::<i32>().unwrap());

    let mabye_num = maybe(num_parser);

    let test_list = &[("633x", Some(Some(633)), "x"), ("X5", Some(None), "X5")];

    for t in test_list {
        let parsed = mabye_num.parse(t.0);
        assert_eq!(parsed.res.ok(), t.1);
        assert_eq!(parsed.other, t.2);
    }
}

#[test]
fn rep_test() {
    let num_parser = pred(|c: &char| c.is_numeric())
                         .greedy()
                         .map(|s: &str| s.parse::<i32>().unwrap());

    let space_parser = pred(|c: &char| c == &' ').greedy().map(|_| ());

    let list_of_nums_sum = rep(num_parser.skip(maybe(space_parser)))
                               .map(|x: Vec<_>| x.iter().fold(0, |acc, &x| acc + x));

    let test_list = &[("5", Some(5), ""),
                      ("1 2 3 4 50  12 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1",
                       Some(100),
                       ""),
                      ("633X", Some(633), "X"),
                      (" 5XA", None, " 5XA"),
                      ("500 20  bar", Some(520), "bar")];

    for t in test_list {
        let parsed = list_of_nums_sum.parse(t.0);
        assert_eq!(parsed.res.ok(), t.1);
        assert_eq!(parsed.other, t.2);
    }
}

#[test]
fn num_parser_test() -> () {
    let num_parser = pred(|c: &char| c.is_numeric()).greedy();

    let num_parser_num = num_parser.map(|x: &str| x.parse::<i32>().unwrap());

    let test_list = &[("633xa", Some(633), "xa"),
                      ("-1", None, "-1"),
                      ("633", Some(633), ""),
                      ("a633xa", None, "a633xa"),
                      ("6_33xa", Some(6), "_33xa"),
                      ("s633a", None, "s633a")];

    for t in test_list {
        let parsed = num_parser_num.parse(t.0);
        assert_eq!(parsed.res.ok(), t.1);
        assert_eq!(parsed.other, t.2);
    }
}



#[test]
fn expr_test() {


    #[derive(PartialEq, Debug, Clone)]
    enum Node {
        Num(i32),
        Add(Box<(Node, Node)>),
        Sub(Box<(Node, Node)>),
        Mul(Box<(Node, Node)>),
        Div(Box<(Node, Node)>),
    }

    impl Node {
        fn create_op(op_symb: char, lhs: Node, rhs: Node) -> Self {
            match op_symb {
                '+' => Node::Add(Self::node_pair(lhs, rhs)),
                '-' => Node::Sub(Self::node_pair(lhs, rhs)),
                '*' => Node::Mul(Self::node_pair(lhs, rhs)),
                '/' => Node::Div(Self::node_pair(lhs, rhs)),
                _ => panic!("{:?} not allowed", op_symb),
            }
        }

        fn node_pair(lhs: Node, rhs: Node) -> Box<(Node, Node)> {
            Box::new((lhs, rhs))
        }

        fn calc(&self) -> i32 {
            match self {
                &Node::Num(n) => n,
                &Node::Add(ref box_pair) => box_pair.0.calc() + box_pair.1.calc(),
                &Node::Mul(ref box_pair) => box_pair.0.calc() * box_pair.1.calc(),
                &Node::Div(ref box_pair) => box_pair.0.calc() / box_pair.1.calc(),
                &Node::Sub(ref box_pair) => box_pair.0.calc() - box_pair.1.calc(),
            }
        }
    }

    fn list_to_tree((f, r): (Node, Option<Vec<(char, Node)>>)) -> Node {
        let mut n = f;
        for &(op,  ref rh) in r.unwrap_or(Vec::new()).iter() {
            n = Node::create_op(op, n, rh.clone());
        }
        return n;
    }


    fn num<'a>(tokens: &'a str) -> ParseResult<Node, &'a str> {
        (pred(|c: &char| c.is_numeric())
             .greedy()
             .skip(maybe(token(' ').greedy()))
             .map(|s: &str| Node::Num(s.parse::<i32>().unwrap())))
            .parse(tokens)
    }


    fn parens_expr<'a>(tokens: &'a str) -> ParseResult<Node, &'a str> {
        (token('(')
             .skip(maybe(token(' ').greedy()))
             .and(fn_parser(add_op).skip(token(')').skip(maybe(token(' ').greedy())))))
            .map(|(_, a)| a)
            .parse(tokens)
    }

    fn mul_op<'a>(tokens: &'a str) -> ParseResult<Node, &'a str> {
        let mul_symb = token('*').skip(maybe(token(' ').greedy()));
        let div_symb = token('/').skip(maybe(token(' ').greedy()));;
        let mul_div = mul_symb.or(div_symb);
        fn_parser(num)
            .or(fn_parser(parens_expr))
            .and(maybe(rep(mul_div.and(fn_parser(num).or(fn_parser(parens_expr))))))
            .map(list_to_tree)
            .parse(tokens)
    }



    assert_eq!(fn_parser(mul_op).parse("018 ").res.unwrap(), Node::Num(18));

    fn add_op<'a>(tokens: &'a str) -> ParseResult<Node, &'a str> {
        let add_symb = token('+').skip(maybe(token(' ').greedy()));
        let sub_symb = token('-').skip(maybe(token(' ').greedy()));;
        let add_sub_symb = add_symb.or(sub_symb);
        fn_parser(mul_op)
            .and(maybe(rep(add_sub_symb.and(fn_parser(mul_op)))))
            .map(list_to_tree)
            .parse(tokens)
    }

    assert_eq!(fn_parser(mul_op).parse("18 /  9  * 3 *1").res.unwrap().calc(),
               6);
    assert_eq!(fn_parser(add_op).parse("1+5 /  9  * 3").res.unwrap(),
               Node::Add(Box::new((Node::Num(1),
                                   Node::Mul(Box::new((Node::Div(Box::new((Node::Num(5),
                                                                           Node::Num(9)))),
                                                       Node::Num(3))))))));

    assert_eq!(fn_parser(add_op)
                   .parse("5+ 18/  (9 - 5 * (2 - (7))) + (20) / 10*2-6*8/3*(7-63)+5/2")
                   .res
                   .unwrap()
                   .calc(),
               5 + 18 / (9 - 5 * (2 - 7)) + 20 / 10 * 2 - 6 * 8 / 3 * (7 - 63) + 5 / 2);




    // Ok((5, vec![('+', 9), ('+', 3)].into_boxed_slice())));
}

#[test]
fn simple_char_test() {
    let x_char = token('x');
    assert_eq!(x_char.parse("xxy").res, Ok('x'));

    // let x_char = token('x');
    let y_char = token('y');

    let xy = x_char.or(y_char.clone())
                   .and(y_char);
    assert_eq!(xy.parse("yyx").res, Ok(('y', 'y')));

    let x_char = token('x').greedy();
    assert_eq!(x_char.parse("xxy").res, Ok("xx"));


}
