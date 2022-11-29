use crate::*;

pub enum NumberSize {
    Normal,
    Long,
    Longer
}

pub enum Token {
    CharLit(char),
    StringLit(String),
    IntLit(u64, NumberSize),
    FloatLit(f64, NumberSize),
    Identifier(String),
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Semicolon,
    Comma,
    Dot,
    Arrow,
    Assign,
    AssignPlus,
    AssignMinus,
    AssignMul,
    AssignDiv,
    AssignRem,
    AssignShl,
    AssignShr,
    AssignAnd,
    AssignXor,
    AssignOr,
    AssignLogAnd,
    AssignLogOr,
    AssignLogNot,
    And,
    Xor,
    Or,
    Not,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Shl,
    Shr,
    Plus,
    Minus,
    Star,
    Div,
    Rem,
    Inc,
    Dec,
    KwdAlignas,
    KwdAlignof,
    KwdAtomic,
    KwdAuto,
    KwdBitInt,
    KwdBool,
    KwdBreak,
    KwdCase,
    KwdChar,
    KwdComplex,
    KwdConst,
    KwdConstexpr,
    KwdContinue,
    KwdDecimal128,
    KwdDecimal32,
    KwdDecimal64,
    KwdDefault,
    KwdDo,
    KwdDouble,
    KwdElse,
    KwdEnum,
    KwdExtern,
    KwdFalse,
    KwdFloat,
    KwdFor,
    KwdGeneric,
    KwdGoto,
    KwdIf,
    KwdImaginary,
    KwdInline,
    KwdInt,
    KwdLong,
    KwdNoreturn,
    KwdNullptr,
    KwdRegister,
    KwdRestrict,
    KwdReturn,
    KwdShort,
    KwdSigned,
    KwdSizeof,
    KwdStatic,
    KwdStaticAssert,
    KwdStruct,
    KwdSwitch,
    KwdThreadLocal,
    KwdTrue,
    KwdTypedef,
    KwdTypeof,
    KwdTypeofUnqual,
    KwdUnion,
    KwdUnsigned,
    KwdVoid,
    KwdVolatile,
    KwdWhile,
}