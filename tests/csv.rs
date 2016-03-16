
extern crate prs;

use prs::pars::{Parse, Token, predicate};
use prs::comb::ParserComb;
use prs::comb::wrap;
use prs::comb::many0;
use prs::comb::eof;
use prs::comb::many;
use prs::result::SupressedRes;
use prs::stream::char_stream::CharStream;


fn csv_parse(input: &str) -> Result<Vec<Vec<String>>, String> {
    let tokens = &mut CharStream::new(input);


    let sep = wrap(Token(','));
    let quote = wrap(Token('"'));
    let dquote = Token('"').and(Token('"')).then(|_: SupressedRes| '"');
    let nl = wrap(Token('\n'));
    let non_spec_char = wrap(predicate(|c: &char| *c != '"' && *c != ',' && *c != '\n')
        .then(|c| {println!("{:?}", c); c }));


    let qstr = (quote.clone(),
                wrap(many0::<_,String>(non_spec_char.clone()
                .or(dquote)
                .or(nl.clone())
                .or(sep.clone()))),
                quote.clone()).then(|(_,s,_)| s);

    let value = wrap(
        qstr
        .or( many0::<_,String>(non_spec_char.clone()))
        );

    let line = wrap((many0(value.clone().skip(sep)),
        value.clone())
        .then(|(mut v, r):(Vec<_>,_)| { v.push(r); println!("{:?}",v); v }));


    many(line.skip(nl)).skip(eof()).parse(tokens).map_err(|e| format!("{}", e))

}

#[test]
fn csv_test() {
    let mut actual: Vec<Vec<String>> = Vec::new();
    actual.push(vec!["Year","Make","Model","Description","Price"]
                .into_iter().map(|s| s.to_owned()).collect());
    actual.push(vec!["1997","Ford","E350","ac, abs, moon","3000.00"]
                .into_iter().map(|s| s.to_owned()).collect());

    actual.push( vec![ "1999","Chevy","Venture \"Extended Edition\"","","4900.00"]
                .into_iter().map(|s| s.to_owned()).collect());
    actual.push( vec![ "1999","Chevy","Venture \"Extended Edition, Very Large\"","","5000.00"]
                .into_iter().map(|s| s.to_owned()).collect());
    actual.push( vec![ "1996","Jeep","Grand Cherokee","MUST SELL!\nair, moon roof, loaded","4799.00"]
                .into_iter().map(|s| s.to_owned()).collect());

    let pars_res = csv_parse(r#"Year,Make,Model,Description,Price
1997,Ford,E350,"ac, abs, moon",3000.00
1999,Chevy,"Venture ""Extended Edition""","",4900.00
1999,Chevy,"Venture ""Extended Edition, Very Large""",,5000.00
1996,Jeep,Grand Cherokee,"MUST SELL!
air, moon roof, loaded",4799.00
"#);

    assert_eq!(pars_res.unwrap(), actual);

}
