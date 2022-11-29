use crate::*;
use crate::errors::Error;
use crate::lex::TokenType::*;
use crate::Standard::*;

#[derive(Debug, PartialEq)]
pub enum NumberSize {
    Normal,
    Long,
    Longer
}

#[derive(Debug, PartialEq)]
pub enum TokenType {
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
    LogAnd,
    LogOr,
    LogNot,
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

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub loc: Location,
}

impl Token {
    fn new(token_type: TokenType, loc: Location) -> Self {
        Self {
            token_type,
            loc,
        }
    }
}

macro_rules! next_char_impl {
    (fatal $self:expr, $func:ident) => {
        match $self.$func() {
            Some(c) => c,
            None => {
                $self.error(&$self.loc, Error::UnexpectedEOF);
                return None
            },
        }
    };
    (done $self:expr, $token:expr, $func:ident) => {
        match $self.$func() {
            Some(c) => c,
            None => return $token,
        }
    };
}

macro_rules! next_char {
    (fatal $self:expr) => { next_char_impl!(fatal $self, next_char) };
    (done $self:expr, $token:expr) => { next_char_impl!(fatal $self, next_char) };
}

macro_rules! next_char_esc {
    (fatal $self:expr) => { next_char_impl!(fatal $self, next_char_esc) };
    (done $self:expr, $token:expr) => { next_char_impl!(fatal $self, next_char_esc) };
}

macro_rules! build_op {
    ($self:expr, $d:ident, $($c:expr, $e:ident),*) => {
        match $self.next_char() {
            Some(c) => match c {
                $(
                    $c => $e,
                )*
                _ => {
                    $self.save_char(c);
                    $d
                },
            },
            None => $d,
        }
    };
}

impl Context {
    fn next_char(&mut self) -> Option<char> {
        if let Some(c) = self.save_char {
            self.save_char = None;
            return Some(c);
        }
        if self.position >= self.input.len() {
            return None;
        }
        let c = self.input[self.position];
        self.position += 1;
        match c {
            '\n' => {
                self.loc.line += 1;
                self.loc.column = 0;
            },
            '\t' => self.loc.column = ((self.loc.column - 2) | 7) + 2,
            _ => self.loc.column += 1,
        }
        Some(c)
    }

    fn save_char(&mut self, c: char) {
        self.save_char = Some(c);
    }

    fn next_char_esc(&mut self) -> Option<(char, bool)> {
        let loc = self.loc.clone();
        let backslash = self.next_char()?;
        match backslash {
            '\\' => {
                let c = next_char!(fatal self);
                match c {
                    'n' => Some(('\n', true)),
                    't' => Some(('\t', true)),
                    'r' => Some(('\r', true)),
                    'a' => Some(('\x07', true)),
                    'b' => Some(('\x08', true)),
                    'v' => Some(('\x11', true)),
                    'f' => Some(('\x12', true)),
                    '\\' | '\'' | '"' | '?' => Some((c, true)),
                    _ => {
                        self.warning(&loc, Error::BadEscapeSequence);
                        Some((c, false))
                    },
                }
            },
            _ => Some((backslash, false)),
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        if let Some(t) = self.tokens.pop() {
            return Some(t);
        }

        loop {
            let mut c = self.next_char()?;
            while c.is_ascii_whitespace() {
                c = self.next_char()?;
            }

            if c.is_ascii_alphabetic() || c == '_' {
                return self.scan_word(c);
            }
            else if c.is_ascii_digit() {
                return self.scan_number(c);
            }
            else if c == '\'' {
                return self.scan_char();
            }
            else if c == '"' {
                return self.scan_string();
            }

            let loc = self.loc.clone();
            return match c {
                '+' => Some(Token::new(build_op! { self, Plus, '+', Inc, '=', AssignPlus }, loc)),
                '-' => Some(Token::new(build_op! { self, Minus, '-', Dec, '=', AssignMinus, '>', Arrow }, loc)),
                '<' => {
                    let mut token = build_op! { self, Lt, '<', Shl, '=', Le };
                    if token == Shl {
                        token = build_op! { self, token, '=', AssignShl };
                    }
                    Some(Token::new(token, loc))
                },
                '>' => {
                    let mut token = build_op! { self, Gt, '>', Shr, '=', Ge };
                    if token == Shr {
                        token = build_op! { self, token, '=', AssignShr };
                    }
                    Some(Token::new(token, loc))
                },
                '&' => Some(Token::new(build_op! { self, And, '&', LogAnd, '=', AssignAnd }, loc)),
                '|' => Some(Token::new(build_op! { self, Or, '|', LogOr, '=', AssignOr }, loc)),
                '^' => Some(Token::new(build_op! { self, Xor, '=', AssignXor }, loc)),
                '=' => Some(Token::new(build_op! { self, Assign, '=', Eq }, loc)),
                '!' => Some(Token::new(build_op! { self, LogNot, '=', Ne }, loc)),
                '*' => Some(Token::new(build_op! { self, Star, '=', AssignMul }, loc)),
                '/' => {
                    match self.next_char() {
                        Some(c) => {
                            match c {
                                '/' => {
                                    while let Some(newline) = self.next_char() {
                                        if newline == '\n' {
                                            break;
                                        }
                                    }
                                    self.save_char(c);
                                    continue;
                                },
                                '=' => Some(Token::new(AssignDiv, loc)),
                                _ => {
                                    self.save_char(c);
                                    Some(Token::new(Div, loc))
                                }
                            }
                        },
                        None => Some(Token::new(Div, loc)),
                    }
                },
                '%' => Some(Token::new(build_op! { self, Rem, '=', AssignRem }, loc)),
                '~' => Some(Token::new(Not, loc)),
                '(' => Some(Token::new(LeftParen, loc)),
                ')' => Some(Token::new(RightParen, loc)),
                '[' => Some(Token::new(LeftBracket, loc)),
                ']' => Some(Token::new(RightBracket, loc)),
                '{' => Some(Token::new(LeftBrace, loc)),
                '}' => Some(Token::new(RightBrace, loc)),
                ';' => Some(Token::new(Semicolon, loc)),
                ',' => Some(Token::new(Comma, loc)),
                '.' => Some(Token::new(Dot, loc)),
                _ => {
                    self.error(&loc, Error::BadToken);
                    continue;
                },
            }
        }
    }

    fn scan_word(&mut self, first: char) -> Option<Token> {
        let loc = self.loc.clone();
        let mut c = first;
        let mut string = String::new();
        loop {
            string.push(c);
            match self.next_char() {
                Some(ch) => c = ch,
                None => break,
            }
            if !c.is_ascii_alphanumeric() && c != '_' {
                self.save_char(c);
                break;
            }
        }
        Some(Token::new(match string.as_str() {
            "alignas" if self.standard >= C23 => KwdAlignas,
            "_Alignas" if self.standard >= C11 => KwdAlignas,
            "alignof" if self.standard >= C23 => KwdAlignof,
            "_Alignof" if self.standard >= C11 => KwdAlignof,
            "_Atomic" if self.standard >= C11 => KwdAtomic,
            "auto" => KwdAuto,
            "_BitInt" => KwdBitInt,
            "bool" if self.standard >= C23 => KwdBool,
            "_Bool" if self.standard >= C99 => KwdBool,
            "break" => KwdBreak,
            "case" => KwdCase,
            "char" => KwdChar,
            "_Complex" if self.standard >= C99 => KwdComplex,
            "const" => KwdConst,
            "constexpr" if self.standard >= C23 => KwdConstexpr,
            "continue" => KwdContinue,
            "_Decimal128" if self.standard >= C23 => KwdDecimal128,
            "_Decimal32" if self.standard >= C23 => KwdDecimal32,
            "_Decimal64" if self.standard >= C23 => KwdDecimal64,
            "default" => KwdDefault,
            "do" => KwdDo,
            "double" => KwdDouble,
            "else" => KwdElse,
            "enum" => KwdEnum,
            "extern" => KwdExtern,
            "false" if self.standard >= C23 => KwdFalse,
            "float" => KwdFloat,
            "for" => KwdFor,
            "_Generic" if self.standard >= C11 => KwdGeneric,
            "goto" => KwdGoto,
            "if" => KwdIf,
            "_Imaginary" if self.standard >= C99 => KwdImaginary,
            "inline" if self.standard >= C99 => KwdInline,
            "int" => KwdInt,
            "long" => KwdLong,
            "_Noreturn" if self.standard >= C11 => KwdNoreturn,
            "nullptr" if self.standard >= C23 => KwdNullptr,
            "register" => KwdRegister,
            "restrict" if self.standard >= C99 => KwdRestrict,
            "return" => KwdReturn,
            "short" => KwdShort,
            "signed" => KwdSigned,
            "sizeof" => KwdSizeof,
            "static" => KwdStatic,
            "static_assert" if self.standard >= C23 => KwdStaticAssert,
            "_Static_assert" if self.standard >= C11 => KwdStaticAssert,
            "struct" => KwdStruct,
            "switch" => KwdSwitch,
            "thread_local" if self.standard >= C23 => KwdThreadLocal,
            "_Thread_local" if self.standard >= C11 => KwdThreadLocal,
            "true" if self.standard >= C23 => KwdTrue,
            "typedef" => KwdTypedef,
            "typeof" if self.standard >= C23 => KwdTypeof,
            "typeof_unqual" if self.standard >= C23 => KwdTypeofUnqual,
            "union" => KwdUnion,
            "unsigned" => KwdUnsigned,
            "void" => KwdVoid,
            "volatile" => KwdVolatile,
            "while" => KwdWhile,
            _ => Identifier(string),
        }, loc))
    }

    fn scan_number(&mut self, first: char) -> Option<Token> {
        let loc = self.loc.clone();
        let mut int_value: u64 = 0;
        let mut float_value: f64 = 0.;
        let mut int_width = NumberSize::Normal;
        let mut float_width = NumberSize::Long;
        let mut use_float = false;
        let mut use_unsigned = false;
        let mut use_hex = false;
        let mut exponent = 0;
        let mut neg_exponent = false;
        let mut c = first;

        if c == '0' {
            c = next_char!(fatal self);
            if c == 'x' || c == 'X' {
                use_hex = true;
                c = next_char!(fatal self);
                while c.is_ascii_hexdigit() {
                    int_value *= 16;
                    if c.is_ascii_digit() {
                        int_value += c as u64 - '0' as u64;
                    }
                    else {
                        int_value += c as u64 - 'a' as u64 + 10;
                    }
                    c = match self.next_char() {
                        Some(c) => c,
                        None => return Some(Token::new(IntLit(int_value, int_width), loc)),
                    };
                }
            }
            else {
                let mut valid = true;
                while c.is_ascii_digit() {
                    int_value *= 8;
                    if c >= '8' {
                        valid = false;
                    }
                    int_value += c as u64 - '0' as u64;
                    c = match self.next_char() {
                        Some(c) => c,
                        None => return Some(Token::new(IntLit(int_value, int_width), loc)),
                    };
                }
                if !valid {
                    self.error(&loc, Error::BadOctalDigit);
                }
            }
        }
        else {
            while c.is_ascii_digit() {
                int_value *= 10;
                int_value += c as u64 - '0' as u64;
                c = match self.next_char() {
                    Some(c) => c,
                    None => return Some(Token::new(IntLit(int_value, int_width), loc)),
                };
            }
        }

        if c == '.' {
            let mut decimal: f64 = 1.;
            float_value = int_value as f64;
            use_float = true;
            c = self.next_char()?;
            if use_hex {
                while c.is_ascii_hexdigit() {
                    decimal /= 16.;
                    if c.is_ascii_digit() {
                        float_value += decimal * (c as i32 - '0' as i32) as f64;
                    }
                    else {
                        float_value += decimal * (c as i32 - 'a' as i32 + 10) as f64;
                    }
                    c = match self.next_char() {
                        Some(c) => c,
                        None => return Some(Token::new(FloatLit(float_value, float_width), loc)),
                    };
                }
            }
            else {
                while c.is_ascii_digit() {
                    decimal /= 10.;
                    float_value += decimal * (c as i32 - '0' as i32) as f64;
                    c = match self.next_char() {
                        Some(c) => c,
                        None => return Some(Token::new(FloatLit(float_value, float_width), loc)),
                    }
                }
            }
        }

        if c == 'e' || c == 'E' || c == 'p' || c == 'P' {
            if !use_float {
                float_value = int_value as f64;
                use_float = true;
            }
            if (use_hex && c != 'p' && c != 'P') || (!use_hex && c != 'e' && c != 'E') {
                self.error(&loc, Error::BadExponent);
            }

            c = next_char!(fatal self);
            if c == '+' {
                c = next_char!(fatal self);
            }
            else if c == '-' {
                neg_exponent = true;
                c = next_char!(fatal self);
            }

            if !c.is_ascii_digit() {
                self.error(&loc, Error::BadExponent);
            }
            let mut eof = false;
            while c.is_ascii_digit() {
                exponent *= 10;
                exponent += c as i32 - '0' as i32;
                c = match self.next_char() {
                    Some(c) => c,
                    None => {
                        eof = true;
                        break;
                    },
                };
            }

            if neg_exponent {
                exponent = -exponent;
            }
            if use_hex {
                float_value *= 2.0_f64.powf(exponent as f64);
            }
            else {
                float_value *= 10.0_f64.powf(exponent as f64);
            }

            if eof {
                return Some(Token::new(FloatLit(float_value, float_width), loc));
            }
        }

        loop {
            match c {
                'l' | 'L' => {
                    if use_float {
                        match float_width {
                            NumberSize::Long => float_width = NumberSize::Longer,
                            _ => self.error(&loc, Error::BadFloatLitSuffix),
                        }
                    }
                    else {
                        match int_width {
                            NumberSize::Normal => int_width = NumberSize::Long,
                            NumberSize::Long => int_width = NumberSize::Longer,
                            _ => self.error(&loc, Error::BadIntLitSuffix),
                        }
                    }
                },
                'u' | 'U' => {
                    if use_float {
                        self.error(&loc, Error::BadFloatLitSuffix);
                    }
                    else if use_unsigned {
                        self.error(&loc, Error::BadIntLitSuffix);
                    }
                    else {
                        use_unsigned = true;
                    }
                },
                'f' | 'F' => {
                    if !use_float {
                        self.error(&loc, Error::BadIntLitSuffix);
                    }
                    else {
                        float_width = NumberSize::Normal;
                    }
                }
                _ => {
                    self.save_char(c);
                    break;
                },
            }
            c = self.next_char()?;
        }
        
        if use_float {
            Some(Token::new(FloatLit(float_value, float_width), loc))
        }
        else {
            Some(Token::new(IntLit(int_value, int_width), loc))
        }
    }

    fn scan_char(&mut self) -> Option<Token> {
        let loc = self.loc.clone();
        let (mut c, mut escape) = next_char_esc!(fatal self);
        if c == '\'' && !escape {
            self.error(&loc, Error::EmptyCharLit);
            return Some(Token::new(CharLit('\0'), loc));
        }

        let value = c;
        (c, escape) = next_char_esc!(fatal self);
        if c == '\'' && !escape {
            return Some(Token::new(CharLit(value), loc));
        }

        self.warning(&loc, Error::MulticharLiteral);
        let mut int_value = value as u64;
        while c != '\'' || escape {
            int_value <<= 8;
            int_value |= c as u64;
            (c, escape) = next_char_esc!(fatal self);
        }
        Some(Token::new(IntLit(int_value, NumberSize::Normal), loc))
    }

    fn scan_string(&mut self) -> Option<Token> {
        let loc = self.loc.clone();
        let (mut c, mut escape) = self.next_char_esc()?;
        let mut string = String::new();
        while c != '"' || escape {
            if !escape && c == '\n' {
                self.error(&loc, Error::NewlineInString);
            }
            string.push(c);
            (c, escape) = next_char_esc!(fatal self);
        }
        Some(Token::new(StringLit(string), loc))
    }
}