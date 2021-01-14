use crate::*;

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
    Tuple(Vec<Type>),
    Unknown,
}

impl Type {
    pub(crate) fn arity(&self) -> Either<FunctionArity, TupleArity> {
        match self {
            Type::Tuple(ref tup) => Either::Right(tup.len()),
            Type::Function(ref types) => {
                let r#type_arity = types
                    .iter()
                    .last()
                    .expect("fn item had no return type")
                    .arity();
                let r#type_arity = match r#type_arity {
                    Either::Left(..) => panic!("return type had function erity; internal error"),
                    Either::Right(num) => num,
                };

                let num_types = types.len() - 1;
                Either::Left((num_types, r#type_arity))
            }
            _ => Either::Right(1),
        }
    }

    fn from_string(a: &str) -> Type {
        match a {
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
        }
    }
    pub(crate) fn from_vec_string(args: Vec<Vec<&str>>) -> Type {
        let args = args
            .into_iter()
            .map(|y| {
                if y.len() == 1 {
                    Type::from_string(y[0])
                } else {
                    // TODO handle parsing nested tuple types
                    Type::Tuple(
                        y.into_iter()
                            .map(|x| Type::from_string(x))
                            .collect::<Vec<_>>(),
                    )
                }
            })
            .collect::<Vec<_>>();

        if args.len() == 1 {
            // then this is not a fn type
            args[0].clone()
        } else {
            Type::Function(args)
        }
    }
}
