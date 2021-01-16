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
}

impl Function {
    fn to_rust_code(&self) -> Rust {
        format!(
            "{}fn {}({}) {} {{ 
{}
}}",
            self.visibility_string(),
            self.name,
            self.args_string(),
            self.return_type_string(),
            self.body
        )
    }

    fn visibility_string(&self) -> String {
        match &self.visibility {
            Public => "pub ",
            Private => "",
            PublicToCrate => "pub(crate) ",
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
