#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    String(String),
    Float(f64),
    Integer(i64),
}
