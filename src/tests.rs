use crate::*;

#[test]
fn lex() {
    let ctx = Context::new("int i = 1;".into(), "<stdin>".into(), Standard::C99);
}