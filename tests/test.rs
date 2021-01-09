use rost::{compile, Declaration, FloatBits, IntegerBits, Type};

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
    check_type(prog.declarations.get("main"), Type::String);
    check_type(
        prog.declarations.get("myfunc"),
        Type::Float(FloatBits::SixtyFour),
    );
    check_type(
        prog.declarations.get("divfunc"),
        Type::Float(FloatBits::SixtyFour),
    );
}

#[test]
fn func_app_return_type() {
    let prog = r#"
    divfunc x y = / 2 5
    main = divfunc(10, 2)
    "#;
    let prog = match compile(prog) {
        Ok(o) => (o),
        Err(e) => {
            println!("{}", e);
            panic!()
        }
    };
    check_type(
        prog.declarations.get("divfunc"),
        Type::Float(FloatBits::SixtyFour),
    );
    check_type(
        prog.declarations.get("main"),
        Type::Float(FloatBits::SixtyFour),
    );
}

/*
#[test]
fn func_app_return_type_2() {
    let prog = r#"
    divfunc x y = / x y
    main = divfunc(10, 2)
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
        Type::Float(FloatBits::SixtyFour)
    );
}
*/

#[test]
fn func_app_return_type_3() {
    let prog = r#"
    divfunc x y = / 20 2
    multfunc x y = divfunc(10,20)
    main = multfunc(10, 10)
    "#;
    let prog = match compile(prog) {
        Ok(o) => (o),
        Err(e) => {
            println!("{}", e);
            panic!()
        }
    };
    check_type(
        prog.declarations.get("main"),
        Type::Float(FloatBits::SixtyFour),
    );
}

#[test]
fn parse_type_annotation_1() {
    // should be able to handle upgrading types to higher precisions
    let prog = r#"
    main :: i64
    main = + 10 2 
    "#;

    let prog = match compile(prog) {
        Ok(o) => (o),
        Err(e) => {
            println!("{}", e);
            panic!()
        }
    };

    check_type(
        prog.declarations.get("main"),
        Type::SignedInteger(IntegerBits::SixtyFour),
    );
}

#[test]
fn parse_type_annotation_2() {
    let prog = r#"
    otherfunc :: i32 => i32
    otherfunc x = + x 2
    main = otherfunc(10)
    "#;

    let prog = match compile(prog) {
        Ok(o) => (o),
        Err(e) => {
            println!("{}", e);
            panic!()
        }
    };

    check_type(
        prog.declarations.get("main"),
        Type::Function(vec![
            Type::SignedInteger(IntegerBits::ThirtyTwo),
            Type::SignedInteger(IntegerBits::ThirtyTwo),
        ]),
    );
}
#[test]
fn parse_type_annotation_3() {
    let prog = r#"
    otherfunc :: i64 => i32 => i32 => String
    otherfunc x y z = "hello"
    main = otherfunc(10, 2, 3)
    "#;

    let prog = match compile(prog) {
        Ok(o) => (o),
        Err(e) => {
            println!("{}", e);
            panic!()
        }
    };

    check_type(
        prog.declarations.get("main"),
        Type::Function(vec![
            Type::SignedInteger(IntegerBits::SixtyFour),
            Type::SignedInteger(IntegerBits::ThirtyTwo),
            Type::SignedInteger(IntegerBits::ThirtyTwo),
            Type::String,
        ]),
    );
}

fn check_type(a: Option<&Declaration>, b: Type) {
    assert_eq!(
        if let Some(Declaration::Expr { value, .. }) = a {
            Some(value.return_type.clone())
        } else {
            None
        },
        Some(b)
    );
}
