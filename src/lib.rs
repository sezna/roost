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
    character::complete::{alphanumeric1, char, multispace0, none_of, one_of},
    combinator::{map, not, recognize},
    error::{context, VerboseError},
    multi::{many0, many1, separated_list0, separated_list1},
    number::complete::recognize_float,
    sequence::{delimited, pair, terminated, tuple},
    IResult,
};
use std::collections::HashMap;
use std::str::FromStr;
use thiserror::Error;

#[derive(PartialEq, Debug, Clone)]
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
    context("name", alphanumeric1)(i)
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
    match (op, lhs.return_type.clone(), rhs.return_type.clone()) {
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
#[derive(PartialEq, Debug, Clone)]
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
    // just a name that gets looked up in the namespace, with precedence to locally scoped vars
    VarExp(String),
}

#[derive(PartialEq, Debug, Clone)]
pub struct TypedExpr {
    expr: Expr,
    pub return_type: Type,
}

impl TypedExpr {
    fn resolve_unknowns(
        &self,
        declarations: &HashMap<String, Declaration>,
    ) -> Result<(TypedExpr, bool), CompileError> {
        // if there is a function application with a known return type, then we can propagate that
        // out
        match self.expr {
            Expr::FuncApp {
                ref func_name,
                args: _,
            } => {
                let _cl = declarations.clone();
                match declarations.get(func_name) {
                    Some(Declaration::Expr { value, .. }) => Ok((
                        TypedExpr {
                            return_type: value.return_type.clone(),
                            expr: self.expr.clone(),
                        },
                        self.return_type != value.return_type,
                    )),
                    None => return Err(CompileError::UnrecognizedFunction(func_name.into())),
                    Some(o) => {
                        return Err(CompileError::CalledNonFunction(
                            func_name.into(),
                            o.type_name(),
                        ))
                    }
                }
            }
            _ => Ok((self.clone(), false)),
        }
    }
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

#[derive(PartialEq, Clone, Debug)]
pub enum Type {
    String,
    SignedInteger(IntegerBits),
    Float(FloatBits),
    Bool,
    UnsignedInteger(IntegerBits),
    Function(Vec<Type>),      // curried type decl, last entry is return type
    Generic { name: String }, // generic/polymorphic type params
    Unknown,
}

impl Type {
    fn from_vec_string(args: Vec<&str>) -> Type {
        let args = args
            .into_iter()
            .map(|x| match x {
                "String" => Type::String,
                "i8" => Type::SignedInteger(IntegerBits::Eight),
                "i16" => Type::SignedInteger(IntegerBits::Sixteen),
                "i32" => Type::SignedInteger(IntegerBits::ThirtyTwo),
                "i64" => Type::SignedInteger(IntegerBits::SixtyFour),
                "i128" => Type::SignedInteger(IntegerBits::OneTwentyEight),
                "u8" => Type::UnsignedInteger(IntegerBits::Eight),
                "u16" => Type::UnsignedInteger(IntegerBits::Sixteen),
                "u32" => Type::UnsignedInteger(IntegerBits::ThirtyTwo),
                "u64" => Type::UnsignedInteger(IntegerBits::SixtyFour),
                "u128" => Type::UnsignedInteger(IntegerBits::OneTwentyEight),
                "f32" => Type::Float(FloatBits::ThirtyTwo),
                "f64" => Type::Float(FloatBits::SixtyFour),
                "usize" => Type::UnsignedInteger(IntegerBits::Arch),
                "isize" => Type::SignedInteger(IntegerBits::Arch),
                "bool" => Type::Bool,
                other => Type::Generic {
                    name: other.to_string(),
                }, /* TODO rest of types */
            })
            .collect::<Vec<_>>();

        if args.len() == 1 {
            args[0].clone()
        } else {
            Type::Function(args)
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Declaration {
    Expr {
        name: String,
        args: Vec<String>,
        value: TypedExpr,
    },
    Trait {
        name: String,
        methods: Vec<TypeAnnotation>,
    },
    TypeAnnotation(TypeAnnotation),
}

impl Declaration {
    fn name(&self) -> String {
        match self {
            Declaration::Expr { name, .. } => name.to_string(),
            Declaration::Trait { name, .. } => name.to_string(),
            Declaration::TypeAnnotation(TypeAnnotation { name, .. }) => name.to_string(),
        }
    }
    fn type_name(&self) -> String {
        match self {
            Declaration::Expr { .. } => "expression",
            Declaration::Trait { .. } => "trait",
            Declaration::TypeAnnotation(..) => "type annotation",
        }
        .into()
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct TypeAnnotation {
    pub name: String,
    pub r#type: Type,
}

#[derive(PartialEq, Debug)]
pub struct Program {
    pub declarations: HashMap<String, Declaration>,
}

impl Program {
    fn contains_main_function(&self) -> bool {
        self.declarations.get("main").is_some()
    }
    fn resolve_unknowns(&mut self) -> Result<(), CompileError> {
        let mut were_changes = true;
        while were_changes {
            were_changes = false;
            let new_decls = self
                .declarations
                .clone()
                .into_iter()
                .map(|(_, decl)| {
                    if let Declaration::Expr { name, args, value } = decl {
                        let (value, were_changes) = value.resolve_unknowns(&self.declarations)?;
                        Ok((Declaration::Expr { args, name, value }, were_changes))
                    } else {
                        Ok((decl, false))
                    }
                })
                .collect::<Result<Vec<_>, CompileError>>()?;
            for (decl, were_changes_2) in new_decls {
                were_changes = were_changes || were_changes_2;
                self.declarations.insert(decl.name(), decl);
            }
        }
        Ok(())
    }
    fn apply_type_annotations(&mut self) -> Result<(), CompileError> {
        let annotations = self
            .declarations
            .clone()
            .into_iter()
            .filter_map(|(name, x)| {
                if let Declaration::TypeAnnotation(annotation) = x {
                    Some((name, annotation))
                } else {
                    None
                }
            });
        for (_name, annotation) in annotations {
            let name = annotation.name.split(' ').collect::<Vec<_>>()[0];
            let to_update = match self.declarations.get_mut(name) {
                Some(o) => o,
                None => return Err(CompileError::AnnotatedNonexistentDeclaration(name.into())),
            };
            match to_update {
                Declaration::Expr { value, .. } => {
                    // TODO: check if it already has a type, and then see if this type can override
                    // it
                    // i.e. i64 can override 132, f64 can override f32
                    value.return_type = annotation.r#type;
                }
                _ => return Err(CompileError::AnnotatedNonAnnotatable(name.into())),
            };
        }
        Ok(())
    }
}

fn parse_program<'a>(i: &'a str) -> IResult<&'a str, Program, VerboseError<&'a str>> {
    let (buf, declarations_vec) = many1(delimited(
        multispace0,
        alt((parse_type_annotation, parse_trait, parse_declaration_expr)),
        multispace0,
    ))(i)?;
    let mut declarations = HashMap::default();
    declarations_vec.into_iter().for_each(|decl| {
        // TODO don't store the names twice
        declarations.insert(decl.name(), decl);
    });
    Ok((buf, Program { declarations }))
}

fn parse_declaration_expr<'a>(i: &'a str) -> IResult<&'a str, Declaration, VerboseError<&'a str>> {
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
        Declaration::Expr {
            name: name.to_string(),
            args: args.iter().map(|x| x.to_string()).collect(),
            value,
        },
    ))
}

fn parse_type_annotation<'a>(i: &'a str) -> IResult<&'a str, Declaration, VerboseError<&'a str>> {
    let (buf, res) = parse_type_annotation_inner(i)?;
    Ok((buf, Declaration::TypeAnnotation(res)))
}

fn parse_type_annotation_inner<'a>(
    i: &'a str,
) -> IResult<&'a str, TypeAnnotation, VerboseError<&'a str>> {
    let (buf, res) = tuple((
        context("type annotation name", parse_name),
        multispace0,
        context("double colon", tuple((char(':'), char(':')))),
        multispace0,
        context(
            "type args",
            separated_list1(
                context(
                    "skinny arrow",
                    tuple((multispace0, char('='), char('>'), multispace0)),
                ),
                parse_name,
            ),
        ),
    ))(i)?;

    let (name, _space1, _colon, _space2, args) = res;
    let annotation = TypeAnnotation {
        name: format!("{} type", name),
        r#type: Type::from_vec_string(args),
    };

    Ok((buf, annotation))
}

fn parse_trait<'a>(i: &'a str) -> IResult<&'a str, Declaration, VerboseError<&'a str>> {
    // should figure out how to use tag! with strings...
    let trait_keyword = tuple((char('t'), char('r'), char('a'), char('i'), char('t')));
    let trait_keyword = delimited(multispace0, trait_keyword, multispace0);
    let opening_brace = delimited(multispace0, char('{'), multispace0);
    let closing_brace = delimited(multispace0, char('}'), multispace0);
    let (buf, res) = tuple((
        trait_keyword,
        parse_name,
        opening_brace,
        many1(delimited(
            multispace0,
            parse_type_annotation_inner,
            multispace0,
        )),
        closing_brace,
    ))(i)?;

    let (_keyword, name, _opening_brace, annotations, _closing_brace) = res;
    Ok((
        buf,
        Declaration::Trait {
            name: name.to_string(),
            methods: annotations,
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
    #[error("You wrote a type annotation for something that doesn't exist or isn't in scope. Declaration \"{0}\" not found.")]
    AnnotatedNonexistentDeclaration(String),
    #[error("Attempted to give a type annotation to something that cannot be annotated. \"{0}\" is not an annotatable expression.")]
    AnnotatedNonAnnotatable(String),
    #[error("Attempted to call something that isn't a function. \"{0}\" is not a function, it is a {1}.")]
    CalledNonFunction(String, String),
}

impl std::convert::From<nom::Err<VerboseError<&str>>> for CompileError {
    fn from(o: nom::Err<VerboseError<&str>>) -> Self {
        CompileError::ParseError(format!("{:#?}", o))
    }
}

fn parse_var_expr<'a>(i: &'a str) -> IResult<&'a str, TypedExpr, VerboseError<&'a str>> {
    let (buf, res) = parse_name(i)?;
    Ok((
        buf,
        TypedExpr {
            expr: Expr::VarExp(res.into()),
            return_type: Type::Unknown,
        },
    ))
}

fn parse_expr<'a>(i: &'a str) -> IResult<&'a str, TypedExpr, VerboseError<&'a str>> {
    context(
        "expression",
        terminated(
            alt((parse_func_app, parse_op_exp, parse_constant, parse_var_expr)),
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
    let (buf, mut prog) = parse_program(input)?;
    if !buf.is_empty() {
        return Err(CompileError::ExtraneousInput(buf.to_string()));
    }
    if !prog.contains_main_function() {
        return Err(CompileError::MissingMainFunction);
    }
    prog.apply_type_annotations()?;
    // TODO: validate all function applications and resolve their `Unknown` return types
    prog.resolve_unknowns()?;

    Ok(prog)
}
