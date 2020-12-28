use rost::{compile, Type};
#[test]
fn basic_prog_1() {
    let prog = "main = + 1 - func(2) 3";
    compile(prog).unwrap();
}
#[test]
fn basic_prog_2() {
    let prog = r#"
myfunc a b = + 1 - func(2) 3
main = myfunc (12, 13)"#;
    match compile(prog) {
        Ok(o) => dbg!(o),
        Err(e) => {
            println!("{}", e);
            panic!()
        }
    };
}

#[test]
fn basic_return_type() {
    let prog = r#"
    myfunc = * 10 2.3
    divfunc = / 10 2
    main = * "multiplied string" 10
    "#;
    let prog = match compile(prog) {
        Ok(o) => (o),
        Err(e) => {
            println!("{}", e);
            panic!()
        }
    };
    assert_eq!(
        prog.declarations.get("main").unwrap().value.return_type,
        Type::String
    );
    assert_eq!(
        prog.declarations.get("myfunc").unwrap().value.return_type,
        Type::Float
    );
    assert_eq!(
        prog.declarations.get("divfunc").unwrap().value.return_type,
        Type::Float
    );
}
