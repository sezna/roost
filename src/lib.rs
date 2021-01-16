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

use either::Either;
use nom::error::VerboseError;
use std::collections::HashMap;
use std::str::FromStr;
use thiserror::Error;

pub(crate) mod declaration;
pub(crate) mod error;
pub(crate) mod expr;
pub(crate) mod literal;
pub(crate) mod operator;
pub(crate) mod parser;
pub(crate) mod prerust;
pub(crate) mod typed_expr;
pub(crate) mod types;

pub use declaration::*;
pub use error::*;
pub use expr::*;
pub use literal::*;
pub use operator::*;
use parser::parse_program;
pub use typed_expr::*;
pub use types::*;

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
                // an expr declaration is a function or value declaration
                Declaration::Expr {
                    value, ref args, ..
                } => {
                    // TODO: check if it already has a type, and then see if this type can override
                    // it
                    // i.e. i64 can override 132, f64 can override f32
                    // check the arity (size of tuple) of the expr in this func
                    let expr_arity = if args.len() == 0 {
                        match value.arity() {
                            Either::Left(..) => todo!("hm"),
                            e @ Either::Right(..) => e,
                        }
                    } else {
                        // if there are args, this is a function
                        // +1 for the return type
                        Either::Left((
                            args.len(),
                            match value.arity() {
                                Either::Left(..) => todo!("hm"),
                                Either::Right(tupl_arity) => tupl_arity,
                            },
                        ))
                    };
                    let annotation_arity = annotation.arity();
                    // when these types are made better, a tuple type of len 1 will be a singleton
                    // type
                    if expr_arity != annotation_arity && {
                        // special case -- if arity is 1 then this could just be a tuple being
                        // identified as a singleton name
                        if let Either::Left((_, ret)) = expr_arity {
                            ret != 1
                        } else {
                            true
                        }
                    } {
                        return Err(CompileError::ArityMismatch {
                            expr_arity: format_arity(expr_arity),
                            annotation_arity: format_arity(annotation_arity),
                        });
                    }

                    value.r#type = annotation.r#type;
                }
                _ => return Err(CompileError::AnnotatedNonAnnotatable(name.into())),
            };
        }
        Ok(())
    }
}

type FunctionArity = (usize, usize);
type TupleArity = usize;

fn format_arity(arity: Either<FunctionArity, TupleArity>) -> String {
    match arity {
        Either::Left((arg_arity, return_type_arity)) => {
            format!("f{}.{}", arg_arity, return_type_arity)
        }
        Either::Right(n) => format!("{}", n),
    }
}

impl TypeAnnotation {
    fn arity(&self) -> Either<FunctionArity, TupleArity> {
        self.r#type.arity()
    }
}

impl std::convert::From<nom::Err<VerboseError<&str>>> for CompileError {
    fn from(o: nom::Err<VerboseError<&str>>) -> Self {
        CompileError::ParseError(format!("{:#?}", o))
    }
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
    prog.resolve_unknowns()?;

    Ok(prog)
}
