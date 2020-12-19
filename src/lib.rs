use nom::{
    branch::alt,
    character::complete::{alpha1, char, multispace0, none_of, one_of},
    combinator::{map, not, recognize},
    error::{context, VerboseError},
    multi::{many0, many1, separated_list0},
    number::complete::recognize_float,
    sequence::{delimited, pair, terminated, tuple},
    IResult,
};
use std::str::FromStr;
use thiserror::Error;

#[derive(PartialEq, Debug)]
pub enum Literal {
    String(String),
    Float(f64),
    Integer(i64),
}

fn parse_op<'a>(i: &'a str) -> IResult<&'a str, Operator, VerboseError<&'a str>> {
    // one_of matches one of the characters we give it
    let (i, t) = terminated(one_of("+-*/"), multispace0)(i)?;

    // because we are matching single character tokens, we can do the matching logic
    // on the returned value
    Ok((
        i,
        match t {
            '+' => Operator::Plus,
            '-' => Operator::Minus,
            '*' => Operator::Multiply,
            '/' => Operator::Divide,
            _ => unreachable!(),
        },
    ))
}
#[derive(PartialEq, Debug)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
}

fn parse_name<'a>(i: &'a str) -> IResult<&'a str, &'a str, VerboseError<&'a str>> {
    context("name", alpha1)(i)
}

fn recognize_base10_int<'a>(input: &'a str) -> IResult<&'a str, &'a str, VerboseError<&'a str>> {
    recognize(tuple((
        many1(one_of("0123456789")),
        not(alt((char('.'), char('e')))),
    )))(input)
}

fn parse_int<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
    let (buff, res) = map(recognize_base10_int, |x| i64::from_str(x).unwrap())(i)?;
    Ok((buff, Expr::Constant(Literal::Integer(res))))
}
fn parse_float<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
    let (buff, res) = map(recognize_float, |x| f64::from_str(x).unwrap())(i)?;
    Ok((buff, Expr::Constant(Literal::Float(res))))
}
fn parse_string<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
    let double_quoted_string = delimited(char('"'), many0(none_of("\"")), char('"'));
    let single_quoted_string = delimited(char('\''), many0(none_of("'")), char('\''));
    let (buf, res) = context("string", alt((double_quoted_string, single_quoted_string)))(i)?;
    Ok((buf, Expr::Constant(Literal::String(res.iter().collect()))))
}
fn parse_constant<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
    context("constant", alt((parse_int, parse_float, parse_string)))(i)
}
fn parse_op_exp<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
    let (buf, (op, lhs, rhs)) = context("op expr", tuple((parse_op, parse_expr, parse_expr)))(i)?;
    Ok((
        buf,
        Expr::OpExp {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            op,
        },
    ))
}
#[derive(PartialEq, Debug)]
pub enum Expr {
    FuncApp {
        func_name: String,
        args: Vec<Expr>,
    },
    Constant(Literal),
    OpExp {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        op: Operator,
    },
}

#[derive(PartialEq, Debug)]
pub struct Declaration {
    name: String,
    args: Vec<String>,
    value: Expr,
}

#[derive(PartialEq, Debug)]
pub struct Program {
    declarations: Vec<Declaration>,
}

impl Program {
    fn contains_main_function(&self) -> bool {
        self.declarations
            .iter()
            .any(|x| x.name == String::from("main"))
    }
}

fn parse_program<'a>(i: &'a str) -> IResult<&'a str, Program, VerboseError<&'a str>> {
    let (buf, declarations) = many1(delimited(multispace0, parse_declaration, multispace0))(i)?;
    Ok((buf, Program { declarations }))
}

fn parse_declaration<'a>(i: &'a str) -> IResult<&'a str, Declaration, VerboseError<&'a str>> {
    let (buf, (name, args, _eq_sign, value)) = context(
        "declaration",
        tuple((
            context("declaration name", terminated(parse_name, multispace0)),
            context(
                "declaration args",
                many0(terminated(parse_name, multispace0)),
            ),
            context(
                "declaration equals sign",
                terminated(char('='), multispace0),
            ),
            context("declaration rhs", parse_expr),
        )),
    )(i)?;

    Ok((
        buf,
        Declaration {
            name: name.to_string(),
            args: args.iter().map(|x| x.to_string()).collect(),
            value,
        },
    ))
}

#[derive(Debug, Error)]
pub enum CompileError {
    #[error("Unrecognized function: \"{0}\"")]
    UnrecognizedFunction(String),
    #[error("Parse error, verbose stack dump: \n{0}\n")]
    ParseError(String),
    #[error("No main function found.")]
    MissingMainFunction,
    #[error("Program contained extraneous input: {0}")]
    ExtraneousInput(String),
}

impl std::convert::From<nom::Err<VerboseError<&str>>> for CompileError {
    fn from(o: nom::Err<VerboseError<&str>>) -> Self {
        CompileError::ParseError(format!("{:#?}", o))
    }
}
fn parse_expr<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
    context(
        "expression",
        terminated(
            alt((parse_func_app, parse_op_exp, parse_constant)),
            multispace0,
        ),
    )(i)
}
fn parse_func_args<'a>(i: &'a str) -> IResult<&'a str, Vec<Expr>, VerboseError<&'a str>> {
    separated_list0(pair(char(','), multispace0), parse_expr)(i)
}
fn parse_func_app<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
    let args_in_parens = context(
        "function arguments",
        delimited(char('('), parse_func_args, char(')')),
    );
    let (rest, (func_name, args)) = context(
        "function application",
        pair(terminated(parse_name, multispace0), args_in_parens),
    )(i)?;

    let func_name = func_name.to_string();
    Ok((rest, Expr::FuncApp { func_name, args }))
}

pub fn compile(input: &str) -> Result<Program, CompileError> {
    let (buf, prog) = parse_program(input)?;
    if !buf.is_empty() {
        return Err(CompileError::ExtraneousInput(buf.to_string()));
    }
    if !prog.contains_main_function() {
        return Err(CompileError::MissingMainFunction);
    }
    Ok(prog)
}

//fn main() {}
