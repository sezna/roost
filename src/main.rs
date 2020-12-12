fn main() {
    println!("Hello, world!");
}

type ParseError = String;
type ParseResult<T> = Result<T, ParseError>;
type MaybeParse<'a> = Option<(Token<'a>, &'a [char], usize)>;
type Tokens<'a> = Vec<Token<'a>>;

const VALID_NAME_CHARS: &[char] = &[
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's',
    't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
    'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
];

#[derive(Debug)]
struct Token<'a> {
    span: (usize, usize),
    value: TokenType<'a>,
}

#[derive(Debug)]
enum TokenType<'a> {
    Operator(&'a char),
    VariableName(&'a [char]),
}

struct GrammarEntry {
    parsing_function: fn(&[char], usize) -> MaybeParse,
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

fn parse_operator<'a>(a: &'a [char], starting_index: usize) -> MaybeParse<'a> {
    assert!(a.len() > 0);
    let tok = TokenType::Operator(match a[0] {
        ref v @ '+' => v,
        ref v @ '-' => v,
        _ => return None,
    });
    let tok = Token {
        span: (starting_index, starting_index + 1),
        value: tok,
    };

    Some((tok, &a[1..], starting_index + 1))
}

fn parse_variable<'a>(a: &'a [char], starting_index: usize) -> MaybeParse<'a> {
    assert!(a.len() > 0);
    dbg!(&a);
    for i in 1..=a.len() {
        if (!VALID_NAME_CHARS.contains(&a[i - 1]) || i == a.len()) {
            let (var_name, remaining_chars) = a.split_at(i);
            let tok = TokenType::VariableName(var_name);
            let tok = Token {
                span: (starting_index, starting_index + var_name.len()),
                value: tok,
            };
            return Some((tok, remaining_chars, starting_index + var_name.len()));
        }
    }

    None
}

fn parse<'a>(input: &'a [char]) -> Result<Tokens, ParseError> {
    let mut tok_buf = Vec::new();
    let mut input_buf = input.clone();
    let mut idx = 0;
    while !input_buf.is_empty() {
        let (input, new_idx) = strip_whitespace(input, idx);
        idx = new_idx;
        let (parsed, remaining_chars, new_idx) = if let Some(o) = try_parse(input_buf, idx) {
            o
        } else {
            return Err(format!("Error at index {}", idx));
        };
        idx = new_idx;
        tok_buf.push(parsed);
        input_buf = remaining_chars;
    }

    Ok(tok_buf)
}

fn try_parse<'a>(input: &'a [char], idx: usize) -> MaybeParse {
    for entry in GRAMMAR {
        if let o @ Some(_) = (entry.parsing_function)(input, idx) {
            return o;
        }
    }
    None
}

fn strip_whitespace<'a>(a: &'a [char], idx: usize) -> (&'a [char], usize) {
    for i in 1..a.len() {
        if ![' ', '\t', '\n'].contains(&a[i]) {
            return (a.split_at(i - 1).1, idx + i);
        }
    }
    (&[], a.len())
}

#[test]
fn test_parse_variable_operator() {
    let prog = "abc+ def".chars().collect::<Vec<_>>();
    let prog = parse(&prog.as_slice()).unwrap();
    assert_eq!(prog.len(), 3);
}
