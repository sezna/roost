use crate::*;
use either::Either;
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
fn parse_type<'a>(i: &'a str) -> IResult<&'a str, Vec<&'a str>, VerboseError<&'a str>> {
    let tuple_type = delimited(
        delimited(multispace0, char('('), multispace0),
        separated_list1(delimited(multispace0, char(','), multispace0), parse_name),
        delimited(multispace0, char(')'), multispace0),
    );
    context("type name", alt((parse_name_to_singleton_vec, tuple_type)))(i)
}
fn parse_name_to_singleton_vec<'a>(
    i: &'a str,
) -> IResult<&'a str, Vec<&'a str>, VerboseError<&'a str>> {
    let (buf, res) = context("name", alphanumeric1)(i)?;
    Ok((buf, vec![res]))
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
            r#type: Type::SignedInteger(IntegerBits::ThirtyTwo),
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
            r#type: Type::Float(FloatBits::SixtyFour),
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
            r#type: Type::String,
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
    match (op, lhs.r#type.clone(), rhs.r#type.clone()) {
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
            r#type: calculated_return_type,
        },
    ))
}
pub(crate) fn parse_program<'a>(i: &'a str) -> IResult<&'a str, Program, VerboseError<&'a str>> {
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
                parse_type,
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
    // ^ I think this is already done?
    Ok((
        rest,
        TypedExpr {
            expr: Expr::FuncApp { func_name, args },
            r#type: Type::Unknown,
        },
    ))
}

fn parse_var_expr<'a>(i: &'a str) -> IResult<&'a str, TypedExpr, VerboseError<&'a str>> {
    let (buf, res) = parse_name(i)?;
    Ok((
        buf,
        TypedExpr {
            expr: Expr::VarExp(res.into()),
            r#type: Type::Unknown,
        },
    ))
}
fn parse_tuple<'a>(i: &'a str) -> IResult<&'a str, TypedExpr, VerboseError<&'a str>> {
    let tuple = delimited(
        delimited(multispace0, char('('), multispace0),
        separated_list1(delimited(multispace0, char(','), multispace0), parse_expr),
        delimited(multispace0, char(')'), multispace0),
    );
    let (buf, res) = context("tuple", tuple)(i)?;

    Ok((
        buf,
        TypedExpr {
            r#type: Type::Tuple(
                res.iter()
                    .map(|TypedExpr { r#type, .. }| r#type.clone())
                    .collect(),
            ),
            expr: Expr::TupleExp(res.into_iter().collect()),
        },
    ))
}

fn parse_expr<'a>(i: &'a str) -> IResult<&'a str, TypedExpr, VerboseError<&'a str>> {
    context(
        "expression",
        terminated(
            alt((
                parse_func_app,
                parse_op_exp,
                parse_constant,
                parse_var_expr,
                parse_tuple,
            )),
            multispace0,
        ),
    )(i)
}
