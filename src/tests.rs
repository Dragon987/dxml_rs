use crate::*;

#[test]
fn literal_parser() {
    let parse_joe = match_literal("Hello Joe!");
    assert_eq!(Ok(("", ())), parse_joe.parse("Hello Joe!"));
    assert_eq!(
        Ok((" Hello Robert!", ())),
        parse_joe.parse("Hello Joe! Hello Robert!")
    );
    assert_eq!(Err("Hello Mike!"), parse_joe.parse("Hello Mike!"));
}

#[test]
fn identifier_parser() {
    assert_eq!(
        Ok(("", "I-am-An-ideNtifier".into())),
        identifier("I-am-An-ideNtifier")
    );
    assert_eq!(
        Ok((" entirely an identifier", "not".into())),
        identifier("not entirely an identifier")
    );
    assert_eq!(
        Err("!not at all an identifier"),
        identifier("!not at all an identifier")
    );
}

#[test]
fn right_combinator() {
    let tag_opener = right(match_literal("<"), identifier);
    assert_eq!(
        Ok(("/>", "my-first-element".into())),
        tag_opener.parse("<my-first-element/>")
    );
    assert_eq!(Err("oops"), tag_opener.parse("oops"));
    assert_eq!(Err("!oops"), tag_opener.parse("<!oops"));
}

#[test]
fn one_or_more_combinator() {
    let parser = one_or_more(match_literal("ha"));

    assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("hahaha"));
    assert_eq!(Err("ahah"), parser.parse("ahah"));
    assert_eq!(Err(""), parser.parse(""));
}

#[test]
fn zero_or_more_combinator() {
    let parser = zero_or_more(match_literal("ha"));

    assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("hahaha"));
    assert_eq!(Ok(("ahah", vec![])), parser.parse("ahah"));
    assert_eq!(Ok(("", vec![])), parser.parse(""));
}

#[test]
fn predicate_combinator() {
    let parser = pred(any_char, |c| *c == 'o');
    assert_eq!(Ok(("mg", 'o')), parser.parse("omg"));
    assert_eq!(Err("lol"), parser.parse("lol"));
}

#[test]
fn quoted_string_parser() {
    assert_eq!(
        Ok(("", "Hello Joe!".into())),
        quoted_string().parse("\"Hello Joe!\"")
    );
}

#[test]
fn attribute_pair() {
    assert_eq!(
        Ok((
            "",
            vec![("one".into(), "1".into()), ("two".into(), "2".into())]
        )),
        attributes().parse(" one=\"1\"  two=\"2\"")
    )
}

#[test]
fn single_element_parser() {
    assert_eq!(
        Ok((
            "",
            Dxml {
                name: "div".into(),
                text: "".into(),
                attributes: vec![("class".into(), "float".into())],
                children: vec![],
            }
        )),
        single_element().parse("<div class=\"float\"/>")
    )
}

#[test]
fn xml_parser() {
    let doc = r#"
    <top label="Top">
        <semi-bottom label="Bottom"/>
        <middle>
            <bottom label="Another Bottom"/>
            <bottom>This is bottom text</bottom>
        </middle>
    </top>
    "#;

    let parsed_doc = Dxml {
        name: "top".into(),
        text: "".into(),
        attributes: vec![("label".into(), "Top".into())],
        children: vec![
            Dxml {
                name: "semi-bottom".into(),
                text: "".into(),
                attributes: vec![("label".into(), "Bottom".into())],
                children: vec![],
            },
            Dxml {
                name: "middle".into(),
                text: "".into(),
                attributes: vec![],
                children: vec![
                    Dxml {
                        name: "bottom".into(),
                        text: "".into(),
                        attributes: vec![("label".into(), "Another Bottom".into())],
                        children: vec![],
                    },
                    Dxml {
                        name: "bottom".into(),
                        text: "This is bottom text".into(),
                        attributes: vec![],
                        children: vec![],
                    },
                ],
            },
        ],
    };

    assert_eq!(parsed_doc, Dxml::from_string(doc).unwrap())
}

#[test]
fn mismatched_closing_tag() {
    let doc = r#"
    <top>
        <bottom/>
    </middle>"#;

    assert_eq!(Err("</middle>"), Dxml::from_string(doc))
}
