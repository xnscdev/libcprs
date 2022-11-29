use crate::*;
use crate::lex::TokenType::*;

fn assert_token(ctx: &mut Context, token_type: TokenType, line: usize, column: usize) {
    let token = ctx.next_token().unwrap();
    assert_eq!(token.token_type, token_type);
    assert_eq!(token.loc.line, line);
    assert_eq!(token.loc.column, column);
}

#[test]
fn simple() {
    let mut ctx = Context::new("int i = 1;".into(), "<stdin>".into(), Standard::C99);
    assert_token(&mut ctx, KwdInt, 1, 1);
    assert_token(&mut ctx, Identifier("i".into()), 1, 5);
    assert_token(&mut ctx, Assign, 1, 7);
    assert_token(&mut ctx, IntLit(1, NumberSize::Normal), 1, 9);
    assert_token(&mut ctx, Semicolon, 1, 10);
}