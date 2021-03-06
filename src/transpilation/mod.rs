use crate::*;

enum Visibility {
    Public,
    PublicToCrate,
    Private,
}
struct Function {
    name: String,
    args: Vec<(String, Type)>,
    body: String, // TODO
    return_type: Option<Type>,
    visibility: Visibility,
    type_args: Vec<String>, // TODO constraints? that's long-tail tho
}

impl Function {
    fn type_args_string(&self) -> Rust {
        if self.type_args.is_empty() {
            String::new()
        } else {
            format!("<{}>", self.type_args.join(", "))
        }
    }
    fn to_rust_code(&self) -> Rust {
        format!(
            "{}fn {}{}({}) {} {{{}}}",
            self.visibility_string(),
            self.name,
            self.type_args_string(),
            self.args_string(),
            self.return_type_string(),
            self.body
        )
    }

    fn visibility_string(&self) -> String {
        match &self.visibility {
            Visibility::Public => "pub ",
            Visibility::Private => "",
            Visibility::PublicToCrate => "pub(crate) ",
        }
        .into()
    }

    fn args_string(&self) -> String {
        self.args
            .iter()
            .map(|(name, ty)| format!("{}: {}", name, ty.to_rust_string()))
            .collect::<Vec<_>>()
            .join(", ")
    }

    fn return_type_string(&self) -> String {
        if let Some(ref ty) = self.return_type {
            format!(" -> {}", ty.to_rust_string())
        } else {
            String::new()
        }
    }
}

type Rust = String;

#[test]
fn test_fn() {
    let func = Function {
        name: "foo".into(),
        args: vec![("a".into(), Type::Bool)],
        body: "return 10;".into(),
        return_type: Some(Type::SignedInteger(IntegerBits::ThirtyTwo)),
        visibility: Visibility::Public,
        type_args: vec!["T".into(), "F".into()],
    };

    assert_eq!(
        func.to_rust_code(),
        "pub fn foo<T, F>(a: bool)  -> i32 {return 10;}"
    );
}
