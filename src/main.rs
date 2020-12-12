fn main() {
    println!("Hello, world!");
}

type ParseError = String;
type ParseResult<T> = Result<T, ParseError>;
type MaybeParse<'a> = Option<(Token<'a>, &'a [char])>;
type Tokens<'a> = Vec<Token<'a>>;

const VALID_NAME_CHARS: &[char] = &[
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's',
    't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
    'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
];
enum TokenType {
    Operator,
}

#[derive(Debug)]
enum Token<'a> {
    Operator(&'a char),
    VariableName(&'a [char]),
}

struct GrammarEntry {
    parsing_function: fn(&[char]) -> MaybeParse,
    name: &'static str,
}

const GRAMMAR: &[GrammarEntry] = &[
    GrammarEntry {
        name: "Operator",
        parsing_function: parse_operator,
    },
    GrammarEntry {
        name: "Variable",
        parsing_function: parse_variable,
    },
];

fn parse_operator<'a>(a: &'a [char]) -> MaybeParse<'a> {
    assert!(a.len() > 0);
    let tok = Token::Operator(match a[0] {
        ref v @ '+' => v,
        ref v @ '-' => v,
        _ => return None,
    });

    Some((tok, &a[1..]))
}

fn parse_variable<'a>(a: &'a [char]) -> MaybeParse<'a> {
    assert!(a.len() > 0);
    dbg!(&a);
    for i in 1..=a.len() {
        if (!VALID_NAME_CHARS.contains(&a[i - 1]) || i == a.len()) {
            let (var_name, remaining_chars) = a.split_at(i);
            let tok = Token::VariableName(var_name);
            return Some((tok, remaining_chars));
        }
    }

    None
}

fn parse<'a>(input: &'a [char]) -> Result<Tokens, ParseError> {
    let mut tok_buf = Vec::new();
    let mut input_buf = input.clone();
    while !input_buf.is_empty() {
        let (parsed, remaining_chars) = if let Some(o) = try_parse(input_buf) {
            o
        } else {
            return Err("yo".into());
        };
        dbg!(&parsed);
        dbg!(&remaining_chars);
        tok_buf.push(parsed);
        input_buf = remaining_chars;
    }

    Ok(tok_buf)
}

fn try_parse<'a>(input: &'a [char]) -> MaybeParse {
    // strip off whitespace
    for entry in GRAMMAR {
        let input = strip_whitespace(input);
        if let o @ Some(_) = (entry.parsing_function)(input) {
            return o;
        }
    }
    None
}

fn strip_whitespace<'a>(a: &'a [char]) -> &'a [char] {
    for i in 0..a.len() {
        if ![' ', '\t', '\n'].contains(&a[i]) {
            return a.split_at(i).1;
        }
    }
    &[]
}

#[test]
fn test_parse_variable_operator() {
    let prog = "abc + def".chars().collect::<Vec<_>>();
    let prog = parse(&prog.as_slice()).unwrap();
    assert_eq!(prog.len(), 3);
}
