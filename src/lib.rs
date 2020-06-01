#[cfg(test)]
mod tests;

/// Represents AST of XML structure
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Dxml {
    /// Holds name of the tag
    pub name: String,
    /// Holds trimmed text contained inside of tag
    pub text: String,
    /// Vector of (attribute_name, attribute_values)
    pub attributes: Vec<(String, String)>,
    /// Vector containing children of current tag
    pub children: Vec<Dxml>,
}

impl Dxml {
    /// Construct new Dxml structure from given document
    ///
    /// #Example
    ///
    ///```
    /// use dxml_rs::Dxml;
    /// let doc = r#"<test label="TEST">
    ///     <child>This is some text!  </child>
    /// </test>"#;
    /// let root = Dxml::from_string(doc);
    /// assert!(root.is_ok());
    /// let root = root.unwrap();
    /// assert_eq!(root, Dxml { name: "test".into(), text: "".into(), attributes: vec![("label".into(), "TEST".into())], children: vec![
    ///     Dxml { name: "child".into(), text: "This is some text!".into(), attributes: vec![], children: vec![] },
    /// ]});
    ///```
    pub fn from_string(doc: &str) -> Result<Self, &str> {
        element().parse(doc).and_then(|(next_input, val)| if next_input == "" { Ok(val) } else { Err(next_input) })
    }
}

/// Holds Parsers on heap in order to speed up compilations
struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
}

impl<'a, Output> BoxedParser<'a, Output> {

    /// Constructs new BoxedParser from simple Parser
    fn new<P>(parser: P) -> Self
    where
        P: Parser<'a, Output> + 'a,
    {
        BoxedParser {
            parser: Box::new(parser),
        }
    }
}

impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self.parser.parse(input)
    }
}

/// Defines results of all parsing functions
type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

/// Defines what every parser must implement
trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;

    /// maps self to BoxedParser
    fn map<F, NewOutput>(self, map_fn: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        F: Fn(Output) -> NewOutput + 'a,
    {
        BoxedParser::new(map(self, map_fn))
    }

    /// 'preds' self to BoxedParser
    fn pred<F>(self, pred_fn: F) -> BoxedParser<'a, Output>
    where
        Self: Sized + 'a,
        Output: 'a,
        F: Fn(&Output) -> bool + 'a,
    {
        BoxedParser::new(pred(self, pred_fn))
    }

    /// Converts Parser<A> to Parser<B> by applying Fn(A) -> Parser<B> to A
    fn and_then<F, NextParser, NewOutput>(self, f: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        NextParser: Parser<'a, NewOutput> + 'a,
        F: Fn(Output) -> NextParser + 'a,
    {
        BoxedParser::new(and_then(self, f))
    }
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> ParseResult<'a, Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

/// Constructs a new function that parses literal provided in 'expected' argument
fn match_literal<'a>(expected: &'static str) -> impl Parser<'a, ()> {
    move |input: &'a str| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

/// Parses current string
fn identifier(input: &str) -> ParseResult<String> {
    let mut matched = String::new();
    let mut chars = input.chars();

    match chars.next() {
        Some(next) if next.is_alphabetic() => matched.push(next),
        _ => return Err(input),
    }

    while let Some(next) = chars.next() {
        if next.is_alphanumeric() || next == '-' {
            matched.push(next);
        } else {
            break;
        }
    }

    let next_idx = matched.len();
    Ok((&input[next_idx..], matched))
}

/// Constructs a parser that parses text after a tag
fn text<'a>() -> impl Parser<'a, String> {
    zero_or_more(any_char.pred(|c| *c != '<'))
        .map(|chars| chars.into_iter().collect::<String>().trim().into())
}

/// Constructs a parser that parses text in quotes
fn quoted_string<'a>() -> impl Parser<'a, String> {
    right(
        match_literal("\""),
        left(
            zero_or_more(any_char.pred(|c| *c != '"')),
            match_literal("\""),
        ),
    )
    .map(|chars| chars.into_iter().collect())
}

/// Constructs a parser that returns vector of attribute pairs
fn attributes<'a>() -> impl Parser<'a, Vec<(String, String)>> {
    zero_or_more(right(space1(), attribute_pair()))
}

fn parent_element<'a>() -> impl Parser<'a, Dxml> {
    open_element().and_then(|el| {
        pair(
            text(),
            left(zero_or_more(element()), close_element(el.name.clone())),
        )
        .map(move |(text, children)| {
            let mut el = el.clone();
            el.children = children;
            el.text = text;
            el
        })
    })
}

fn element<'a>() -> impl Parser<'a, Dxml> {
    whitespace_wrap(either(single_element(), parent_element()))
}

fn close_element<'a>(expected_name: String) -> impl Parser<'a, String> {
    right(match_literal("</"), left(identifier, match_literal(">")))
        .pred(move |name| name == &expected_name)
}

fn single_element<'a>() -> impl Parser<'a, Dxml> {
    left(element_start(), match_literal("/>")).map(|(name, attributes)| Dxml {
        name,
        text: "".into(),
        attributes,
        children: vec![],
    })
}

fn open_element<'a>() -> impl Parser<'a, Dxml> {
    left(element_start(), match_literal(">")).map(|(name, attributes)| Dxml {
        name,
        text: "".into(),
        attributes,
        children: vec![],
    })
}

fn element_start<'a>() -> impl Parser<'a, (String, Vec<(String, String)>)> {
    right(match_literal("<"), pair(identifier, attributes()))
}

fn attribute_pair<'a>() -> impl Parser<'a, (String, String)> {
    pair(identifier, right(match_literal("="), quoted_string()))
}

fn space1<'a>() -> impl Parser<'a, Vec<char>> {
    one_or_more(whitespace_char())
}

fn space0<'a>() -> impl Parser<'a, Vec<char>> {
    zero_or_more(whitespace_char())
}

fn whitespace_wrap<'a, P, A>(parser: P) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
{
    right(space0(), left(parser, space0()))
}

fn whitespace_char<'a>() -> impl Parser<'a, char> {
    pred(any_char, |c| c.is_whitespace())
}

fn any_char(input: &str) -> ParseResult<char> {
    input
        .chars()
        .next()
        .map_or(Err(input), |next| Ok((&input[next.len_utf8()..], next)))
}

fn pred<'a, P, A, F>(parser: P, predicate: F) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
    F: Fn(&A) -> bool,
{
    move |input| {
        parser.parse(input).and_then(|(next_input, value)| {
            if predicate(&value) {
                Ok((next_input, value))
            } else {
                Err(input)
            }
        })
    }
}

fn and_then<'a, P, F, A, B, NextP>(parser: P, f: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    NextP: Parser<'a, B>,
    F: Fn(A) -> NextP,
{
    move |input| {
        parser
            .parse(input)
            .and_then(|(next_input, result)| f(result).parse(next_input))
    }
}

fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |input| {
        let (mut input, mut result) = parser.parse(input).map(|(n, f)| (n, vec![f]))?;

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        parser1
            .parse(input)
            .and_then(|(n, r1)| parser2.parse(n).map(|(f, r2)| (f, (r1, r2))))
    }
}

fn either<'a, P1, P2, A>(parser1: P1, parser2: P2) -> impl Parser<'a, A>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, A>,
{
    move |input| match parser1.parse(input) {
        ok @ Ok(_) => ok,
        Err(_) => parser2.parse(input),
    }
}

fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input| parser.parse(input).map(|(n, r)| (n, map_fn(r)))
}

fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(left, _right)| left)
}

fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_left, right)| right)
}
