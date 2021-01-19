use thiserror::Error;
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
    #[error("Attempted to annotate expression of arity {expr_arity} with annotation of arity {annotation_arity}")]
    ArityMismatch {
        expr_arity: String,
        annotation_arity: String,
    },
}
