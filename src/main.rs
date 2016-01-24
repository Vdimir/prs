//
//
//
//
//

use std::marker::PhantomData;
// use  std::iter::FromIterator;
use std::default::Default;

type ParseErrorT =  ();

trait Parser<Tokens> {
    type ParsedDataType;
    // type Tokens;
    // type ParseResult = Result<Self::ParsedDataType, ParseErrorT>;
    fn parse(&self, tokens: Tokens) ->
        (Result<Self::ParsedDataType, ParseErrorT>, Tokens);

    fn map<F>(self, f: F) -> MapedParser<F, Self> where Self: Sized {
        MapedParser {
            f: f,
            parser: self
        }
    }
}


pub struct MapedParser<F, P> {
    f: F,
    parser: P,
}

impl<F, B, P, Tokens> Parser<Tokens> for MapedParser<F,P>
where
    P: Parser<Tokens>,
    F: Fn(P::ParsedDataType) -> B,
{
    type ParsedDataType = B;

    fn parse(&self, tokens: Tokens) ->
            (Result<Self::ParsedDataType, ParseErrorT>, Tokens) {
        let orgiginal = self.parser.parse(tokens);
        return (orgiginal.0.map(&self.f), orgiginal.1);
    }

}


// хранить замыкание в поле структуры или создать trait и каждый раз перегружать метод?
struct PredicateParser<F, T>
    where F: Fn(T) -> bool {
    valid_cheker: F,
    _phantom: PhantomData<T>
}

impl<F, T> PredicateParser<F, T>
    where F: Fn(T) -> bool
{
    fn new(f: F) -> Self {
        PredicateParser { valid_cheker: f, _phantom: PhantomData }
    }

    fn is_valid(&self,arg: T) -> bool {
        (&self.valid_cheker)(arg)
    }
}

type CharsParser<'a, F> = PredicateParser<F, &'a char>;

impl<'a, F> Parser<&'a str> for CharsParser<'a, F>
where F: Fn(&char) -> bool {
// where F: for<'r> Fn(&'r char) -> bool {

    type ParsedDataType = String;
    fn parse(&self, tokens: &'a str) -> (Result<Self::ParsedDataType, ParseErrorT>, &'a str) {

        let mut final_parsed_ind = 0;
        let res_str = tokens.char_indices()
            // .take(2)
            .take_while(|&(_,c)| self.is_valid(&c))
            .map(|(i,c)| { final_parsed_ind = i+1; return c; })
            .collect::<String>();

        if final_parsed_ind > 0 {
           return (Ok(res_str), &tokens[final_parsed_ind..]);
        }
        return (Err(ParseErrorT::default()), tokens);
    }
}



// impl<'a, F, Tokens, TokenT>
//     Parser<Tokens> for PredicateParser<F, &'a TokenT>
// where
//     F: Fn(&TokenT) -> bool,
//     Tokens: IntoIterator<Item=TokenT> + FromIterator<TokenT>
// {
//     type ParsedDataType = Result<TokenT, PhantomData<()>>;

//     fn parse(&self, tokens: Tokens) -> (Self::ParsedDataType, Tokens) {
//         let mut it = tokens.into_iter();
//         let val = it.by_ref().take_while(&self.f).next();

//         return (val.ok_or(PhantomData), it.collect())
//     }
// }



fn main() {


    let num_parser = CharsParser::new(|c:&char| c.is_numeric());

    let num_parser_num = num_parser
        .map(|x:String| x.parse::<i32>().unwrap());
    
    let test_list = &[
        ("633xa",(Ok(633),"xa")),
        ("-1",(Err(()),"-1")),
        ("633",(Ok(633),"")),
        ("a633xa",(Err(()),"a633xa")),
        ("6_33xa",(Ok(6),"_33xa")),
        ("s633a", (Err(()),"s633a")),
    ];

    for t in test_list {
        let res = num_parser_num.parse(t.0);
        println!("{} {:?}", t.0, res);
        assert_eq!(res, t.1);
    }
    
}




// fn prs<P: Parser<T,R>, T, R>(p: P, t: R) -> ParseRes<T,R> {
//     return p.parse(t);
// }
