use crate::*;
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
    pub(crate) fn name(&self) -> String {
        match self {
            Declaration::Expr { name, .. } => name.to_string(),
            Declaration::Trait { name, .. } => name.to_string(),
            Declaration::TypeAnnotation(TypeAnnotation { name, .. }) => name.to_string(),
        }
    }
    pub(crate) fn type_name(&self) -> String {
        match self {
            Declaration::Expr { .. } => "expression",
            Declaration::Trait { .. } => "trait",
            Declaration::TypeAnnotation(..) => "type annotation",
        }
        .into()
    }
}
