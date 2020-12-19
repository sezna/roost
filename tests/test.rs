use rost::compile;
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
        Ok(_) => (),
        Err(e) => {
            println!("{}", e);
            panic!()
        }
    };
}
