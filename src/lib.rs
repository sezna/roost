// there will be a sort of statically-typed but dynamically-feeling type system in the functions
// unknown types are evaluated lazily, at do not define the type of a function. instead, if a
// function has an unknown type, as long as the functions it is called with are actually compatible
// with the function itself, it will be statically verified.
//
// i.e.
//
// myfunc x y = x * y
//
// myfunc(10, 2) // OK, b/c integer * integer is ok, return type is integer
// myfunc("hello", 1) // OK b/c integer * string is ok, return type is string
// myfunc("hi", "yes") // err at compile time: can't multiply string and string
//
// static typing but inferred polymorphism

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
use std::collections::HashMap;
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
#[derive(PartialEq, Debug, Clone, Copy)]
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

// TODO parse suffix type ascriptions, i.e. 10u32 or 10usize
fn parse_int<'a>(i: &'a str) -> IResult<&'a str, TypedExpr, VerboseError<&'a str>> {
    let (buff, res) = map(recognize_base10_int, |x| i64::from_str(x).unwrap())(i)?;
    Ok((
        buff,
        TypedExpr {
            expr: Expr::Constant(Literal::Integer(res)),
            return_type: Type::SignedInteger(IntegerBits::ThirtyTwo),
        },
    ))
}

// TODO: parse f64 and f32 as suffix type ascriptions, like in rust
// i.e. 1.3f64
fn parse_float<'a>(i: &'a str) -> IResult<&'a str, TypedExpr, VerboseError<&'a str>> {
    let (buff, res) = map(recognize_float, |x| f64::from_str(x).unwrap())(i)?;
    Ok((
        buff,
        TypedExpr {
            expr: Expr::Constant(Literal::Float(res)),
            return_type: Type::Float(FloatBits::SixtyFour),
        },
    ))
}
fn parse_string<'a>(i: &'a str) -> IResult<&'a str, TypedExpr, VerboseError<&'a str>> {
    let double_quoted_string = delimited(char('"'), many0(none_of("\"")), char('"'));
    let single_quoted_string = delimited(char('\''), many0(none_of("'")), char('\''));
    let (buf, res) = context("string", alt((double_quoted_string, single_quoted_string)))(i)?;
    Ok((
        buf,
        TypedExpr {
            expr: Expr::Constant(Literal::String(res.iter().collect())),
            return_type: Type::String,
        },
    ))
}
fn parse_constant<'a>(i: &'a str) -> IResult<&'a str, TypedExpr, VerboseError<&'a str>> {
    context("constant", alt((parse_int, parse_float, parse_string)))(i)
}

fn op_type_lookup(lhs: &TypedExpr, rhs: &TypedExpr, op: Operator) -> Type {
    use Operator::*;
    use Type::*;
    // TODO not nearly even kind of exhaustive
    match (op, lhs.return_type, rhs.return_type) {
        (Divide, SignedInteger(_), SignedInteger(_))
        | (Divide, Float(_), SignedInteger(_))
        | (Divide, SignedInteger(_), Float(_))
        | (Multiply, Float(_), SignedInteger(_))
        | (Multiply, SignedInteger(_), Float(_)) => Float(FloatBits::SixtyFour),
        (Multiply, String, SignedInteger(_)) | (Multiply, SignedInteger(_), String) => String,
        (_, l, r) if l == r => l,
        _ => Unknown,
    }
}

fn parse_op_exp<'a>(i: &'a str) -> IResult<&'a str, TypedExpr, VerboseError<&'a str>> {
    let (buf, (op, lhs, rhs)) = context("op expr", tuple((parse_op, parse_expr, parse_expr)))(i)?;
    let calculated_return_type = op_type_lookup(&lhs, &rhs, op);
    Ok((
        buf,
        TypedExpr {
            expr: Expr::OpExp {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op,
            },
            return_type: calculated_return_type,
        },
    ))
}
#[derive(PartialEq, Debug)]
pub enum Expr {
    FuncApp {
        func_name: String,
        args: Vec<TypedExpr>,
    },
    Constant(Literal),
    OpExp {
        lhs: Box<TypedExpr>,
        rhs: Box<TypedExpr>,
        op: Operator,
    },
}

#[derive(PartialEq, Debug)]
pub struct TypedExpr {
    expr: Expr,
    pub return_type: Type,
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum IntegerBits {
    Eight,
    Sixteen,
    ThirtyTwo,
    SixtyFour,
    OneTwentyEight,
    Arch,
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum FloatBits {
    ThirtyTwo,
    SixtyFour,
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum Type {
    String,
    SignedInteger(IntegerBits),
    Float(FloatBits),
    UnsignedInteger(IntegerBits),
    Unknown,
}
#[derive(PartialEq, Debug)]
pub struct Declaration {
    name: String,
    args: Vec<String>,
    pub value: TypedExpr,
}

#[derive(PartialEq, Debug)]
pub struct Program {
    pub declarations: HashMap<String, Declaration>,
}

impl Program {
    fn contains_main_function(&self) -> bool {
        self.declarations.get("main").is_some()
    }
}

fn parse_program<'a>(i: &'a str) -> IResult<&'a str, Program, VerboseError<&'a str>> {
    let (buf, declarations_vec) = many1(delimited(multispace0, parse_declaration, multispace0))(i)?;
    let mut declarations = HashMap::default();
    declarations_vec.into_iter().for_each(|decl| {
        // TODO don't store the names twice
        declarations.insert(decl.name.clone(), decl);
    });
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
fn parse_expr<'a>(i: &'a str) -> IResult<&'a str, TypedExpr, VerboseError<&'a str>> {
    context(
        "expression",
        terminated(
            alt((parse_func_app, parse_op_exp, parse_constant)),
            multispace0,
        ),
    )(i)
}
fn parse_func_args<'a>(i: &'a str) -> IResult<&'a str, Vec<TypedExpr>, VerboseError<&'a str>> {
    separated_list0(pair(char(','), multispace0), parse_expr)(i)
}
fn parse_func_app<'a>(i: &'a str) -> IResult<&'a str, TypedExpr, VerboseError<&'a str>> {
    let args_in_parens = context(
        "function arguments",
        delimited(char('('), parse_func_args, char(')')),
    );
    let (rest, (func_name, args)) = context(
        "function application",
        pair(terminated(parse_name, multispace0), args_in_parens),
    )(i)?;

    let func_name = func_name.to_string();
    // TODO function return types based on lookup table from function declarations
    Ok((
        rest,
        TypedExpr {
            expr: Expr::FuncApp { func_name, args },
            return_type: Type::Unknown,
        },
    ))
}

pub fn compile(input: &str) -> Result<Program, CompileError> {
    let (buf, prog) = parse_program(input)?;
    if !buf.is_empty() {
        return Err(CompileError::ExtraneousInput(buf.to_string()));
    }
    if !prog.contains_main_function() {
        return Err(CompileError::MissingMainFunction);
    }
    // TODO: validate all function applications and resolve their `Unknown` return types
    //
    Ok(prog)
}

//fn main() {}
