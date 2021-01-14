use crate::*;

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
    TupleExp(Vec<TypedExpr>),
}

impl Expr {
    /*
    fn arity(&self) -> TupleArity {
        match self {
            Expr::TupleExp(ref tup) => tup.len(),
            Expr::FuncApp {
                ref func_name,
                args,
                ..
            } => {
                // args + return type is the arity
                todo!("Maybe move this to typed expr? need return type for arity")
            }
            Expr::Constant(..) => 1,
            Expr::VarExp { .. } => todo!("check arity of variable, or assign it here"),
            Expr::OpExp { .. } => 1, // I think this is correct
        }
    }
    */
}
