use crate::*;
#[derive(PartialEq, Debug, Clone)]
pub struct TypedExpr {
    pub expr: Expr,
    pub r#type: Type,
}

impl TypedExpr {
    pub(crate) fn arity(&self) -> Either<FunctionArity, TupleArity> {
        match &self.expr {
            Expr::TupleExp(ref tup) => {
                assert_eq!(
                    if let Type::Tuple(ref stuff) = self.r#type {
                        stuff.len()
                    } else {
                        0
                    },
                    tup.len()
                );
                Either::Right(tup.len())
            }
            Expr::FuncApp {
                ref func_name,
                args,
                ..
            } => {
                // args + return type is the arity
                todo!("Maybe move this to typed expr? need return type for arity")
            }
            Expr::Constant(..) => Either::Right(1),
            Expr::VarExp { .. } => self.r#type.arity(),
            Expr::OpExp { .. } => self.r#type.arity(), // I think this is correct
        }
    }
    pub(crate) fn resolve_unknowns(
        &self,
        declarations: &HashMap<String, Declaration>,
    ) -> Result<(TypedExpr, bool), CompileError> {
        // if there is a function application with a known return type, then we can propagate that
        // out
        match self.expr {
            Expr::FuncApp { ref func_name, .. } => match declarations.get(func_name) {
                Some(Declaration::Expr { value, .. }) => Ok((
                    TypedExpr {
                        r#type: value.r#type.clone(),
                        expr: self.expr.clone(),
                    },
                    self.r#type != value.r#type,
                )),
                None => return Err(CompileError::UnrecognizedFunction(func_name.into())),
                Some(o) => {
                    return Err(CompileError::CalledNonFunction(
                        func_name.into(),
                        o.type_name(),
                    ))
                }
            },
            Expr::TupleExp(ref tupl) => {
                // check type of surrounding expression and apply it to the tuple, if the arity
                // matches
                let (return_expr_arity, return_type) =
                    if let Type::Function(ref inner) = self.r#type {
                        let last_type = inner.iter().last().unwrap();
                        if let Either::Right(num) = last_type.arity() {
                            (num, last_type)
                        } else {
                            panic!("This shouldn't be a fn i think");
                        }
                    } else {
                        dbg!(&self.r#type);
                        todo!("Compile error: tuple exp has non-tuple return type");
                    };
                let return_type = match return_type {
                    Type::Tuple(tupl) => tupl,
                    _ => todo!("internal compiler error fn didn't return tuple"),
                };
                if return_expr_arity != tupl.len() {
                    // TODO this could result from an inferencing error and not an annotation
                    // error. A better compile error could be used here.
                    return Err(CompileError::ArityMismatch {
                        expr_arity: format_arity(Either::Right(tupl.len())),
                        annotation_arity: format_arity(self.r#type.arity()),
                    });
                } else {
                    Ok((
                        TypedExpr {
                            r#type: self.r#type.clone(),
                            expr: Expr::TupleExp(
                                tupl.iter()
                                    .zip(return_type)
                                    .map(|(expr, ty)| {
                                        let mut expr = expr.clone();
                                        expr.r#type = ty.clone();
                                        expr
                                    })
                                    .collect(),
                            ),
                        },
                        tupl.iter().find(|x| x.r#type == Type::Unknown).is_some(),
                    ))
                }
            }
            _ => Ok((self.clone(), false)),
        }
    }
}
