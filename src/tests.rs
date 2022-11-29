use crate::*;
use crate::lex::TokenType::*;

fn assert_token(ctx: &mut Context, token_type: TokenType, line: usize, column: usize) {
    let token = ctx.next_token().unwrap();
    assert_eq!(token.token_type, token_type);
    assert_eq!(token.loc.line, line);
    assert_eq!(token.loc.column, column);
}

#[test]
fn simple_lex() {
    let mut ctx = Context::new("int i = 1;".into(), "<stdin>".into(), Standard::C99);
    assert_token(&mut ctx, KwdInt, 1, 1);
    assert_token(&mut ctx, Identifier("i".into()), 1, 5);
    assert_token(&mut ctx, Assign, 1, 7);
    assert_token(&mut ctx, IntLit(1, NumberSize::Normal), 1, 9);
    assert_token(&mut ctx, Semicolon, 1, 10);
}

#[test]
fn ops_lex() {
    let mut ctx = Context::new("++<<<<=>====".into(), "<stdin>".into(), Standard::C99);
    assert_token(&mut ctx, Inc, 1, 1);
    assert_token(&mut ctx, Shl, 1, 3);
    assert_token(&mut ctx, AssignShl, 1, 5);
    assert_token(&mut ctx, Ge, 1, 8);
    assert_token(&mut ctx, Eq, 1, 10);
    assert_token(&mut ctx, Assign, 1, 12);
}

#[test]
fn comment_lex() {
    let mut ctx = Context::new("/ /* this should be ignored */ % // and this too but not\nthis".into(), "<stdin>".into(), Standard::C99);
    assert_token(&mut ctx, Div, 1, 1);
    assert_token(&mut ctx, Rem, 1, 32);
    assert_token(&mut ctx, Identifier("this".into()), 2, 1);
}