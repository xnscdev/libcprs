use crate::*;
use crate::errors::Error;
use crate::lex::Token::*;
use crate::Standard::*;

pub enum NumberSize {
    Normal,
    Long,
    Longer
}

pub enum Token {
    CharLit(Location, char),
    StringLit(Location, String),
    IntLit(Location, u64, NumberSize),
    FloatLit(Location, f64, NumberSize),
    Identifier(Location, String),
    LeftParen(Location),
    RightParen(Location),
    LeftBracket(Location),
    RightBracket(Location),
    LeftBrace(Location),
    RightBrace(Location),
    Semicolon(Location),
    Comma(Location),
    Dot(Location),
    Arrow(Location),
    Assign(Location),
    AssignPlus(Location),
    AssignMinus(Location),
    AssignMul(Location),
    AssignDiv(Location),
    AssignRem(Location),
    AssignShl(Location),
    AssignShr(Location),
    AssignAnd(Location),
    AssignXor(Location),
    AssignOr(Location),
    AssignLogAnd(Location),
    AssignLogOr(Location),
    AssignLogNot(Location),
    And(Location),
    Xor(Location),
    Or(Location),
    Not(Location),
    Eq(Location),
    Ne(Location),
    Lt(Location),
    Le(Location),
    Gt(Location),
    Ge(Location),
    Shl(Location),
    Shr(Location),
    Plus(Location),
    Minus(Location),
    Star(Location),
    Div(Location),
    Rem(Location),
    Inc(Location),
    Dec(Location),
    KwdAlignas(Location),
    KwdAlignof(Location),
    KwdAtomic(Location),
    KwdAuto(Location),
    KwdBitInt(Location),
    KwdBool(Location),
    KwdBreak(Location),
    KwdCase(Location),
    KwdChar(Location),
    KwdComplex(Location),
    KwdConst(Location),
    KwdConstexpr(Location),
    KwdContinue(Location),
    KwdDecimal128(Location),
    KwdDecimal32(Location),
    KwdDecimal64(Location),
    KwdDefault(Location),
    KwdDo(Location),
    KwdDouble(Location),
    KwdElse(Location),
    KwdEnum(Location),
    KwdExtern(Location),
    KwdFalse(Location),
    KwdFloat(Location),
    KwdFor(Location),
    KwdGeneric(Location),
    KwdGoto(Location),
    KwdIf(Location),
    KwdImaginary(Location),
    KwdInline(Location),
    KwdInt(Location),
    KwdLong(Location),
    KwdNoreturn(Location),
    KwdNullptr(Location),
    KwdRegister(Location),
    KwdRestrict(Location),
    KwdReturn(Location),
    KwdShort(Location),
    KwdSigned(Location),
    KwdSizeof(Location),
    KwdStatic(Location),
    KwdStaticAssert(Location),
    KwdStruct(Location),
    KwdSwitch(Location),
    KwdThreadLocal(Location),
    KwdTrue(Location),
    KwdTypedef(Location),
    KwdTypeof(Location),
    KwdTypeofUnqual(Location),
    KwdUnion(Location),
    KwdUnsigned(Location),
    KwdVoid(Location),
    KwdVolatile(Location),
    KwdWhile(Location),
}

macro_rules! next_char {
    (fatal $self:expr, $loc:expr) => {
        match $self.next_char() {
            Some(c) => c,
            None => {
                $self.error($loc, Error::UnexpectedEOF);
                None?
            },
        }
    };
    (done $self:expr, $token:expr) => {
        match $self.next_char() {
            Some(c) => c,
            None => return $token,
        }
    };
}

impl Context {
    fn next_char(&mut self) -> Option<char> {
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

    fn save_char(&mut self) {
        self.position -= 1;
    }

    fn next_char_esc(&mut self) -> Option<(char, bool)> {
        let loc = self.loc.clone();
        let backslash = self.next_char()?;
        match backslash {
            '\\' => {
                let c = next_char!(fatal self, &loc);
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
        None
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
                self.save_char();
                break;
            }
        }
        Some(match string.as_str() {
            "alignas" if self.standard >= C23 => KwdAlignas(loc),
            "_Alignas" if self.standard >= C11 => KwdAlignas(loc),
            "alignof" if self.standard >= C23 => KwdAlignof(loc),
            "_Alignof" if self.standard >= C11 => KwdAlignof(loc),
            "_Atomic" if self.standard >= C11 => KwdAtomic(loc),
            "auto" => KwdAuto(loc),
            "_BitInt" => KwdBitInt(loc),
            "bool" if self.standard >= C23 => KwdBool(loc),
            "_Bool" if self.standard >= C99 => KwdBool(loc),
            "break" => KwdBreak(loc),
            "case" => KwdCase(loc),
            "char" => KwdChar(loc),
            "_Complex" if self.standard >= C99 => KwdComplex(loc),
            "const" => KwdConst(loc),
            "constexpr" if self.standard >= C23 => KwdConstexpr(loc),
            "continue" => KwdContinue(loc),
            "_Decimal128" if self.standard >= C23 => KwdDecimal128(loc),
            "_Decimal32" if self.standard >= C23 => KwdDecimal32(loc),
            "_Decimal64" if self.standard >= C23 => KwdDecimal64(loc),
            "default" => KwdDefault(loc),
            "do" => KwdDo(loc),
            "double" => KwdDouble(loc),
            "else" => KwdElse(loc),
            "enum" => KwdEnum(loc),
            "extern" => KwdExtern(loc),
            "false" if self.standard >= C23 => KwdFalse(loc),
            "float" => KwdFloat(loc),
            "for" => KwdFor(loc),
            "_Generic" if self.standard >= C11 => KwdGeneric(loc),
            "goto" => KwdGoto(loc),
            "if" => KwdIf(loc),
            "_Imaginary" if self.standard >= C99 => KwdImaginary(loc),
            "inline" if self.standard >= C99 => KwdInline(loc),
            "int" => KwdInt(loc),
            "long" => KwdLong(loc),
            "_Noreturn" if self.standard >= C11 => KwdNoreturn(loc),
            "nullptr" if self.standard >= C23 => KwdNullptr(loc),
            "register" => KwdRegister(loc),
            "restrict" if self.standard >= C99 => KwdRestrict(loc),
            "return" => KwdReturn(loc),
            "short" => KwdShort(loc),
            "signed" => KwdSigned(loc),
            "sizeof" => KwdSizeof(loc),
            "static" => KwdStatic(loc),
            "static_assert" if self.standard >= C23 => KwdStaticAssert(loc),
            "_Static_assert" if self.standard >= C11 => KwdStaticAssert(loc),
            "struct" => KwdStruct(loc),
            "switch" => KwdSwitch(loc),
            "thread_local" if self.standard >= C23 => KwdThreadLocal(loc),
            "_Thread_local" if self.standard >= C11 => KwdThreadLocal(loc),
            "true" if self.standard >= C23 => KwdTrue(loc),
            "typedef" => KwdTypedef(loc),
            "typeof" if self.standard >= C23 => KwdTypeof(loc),
            "typeof_unqual" if self.standard >= C23 => KwdTypeofUnqual(loc),
            "union" => KwdUnion(loc),
            "unsigned" => KwdUnsigned(loc),
            "void" => KwdVoid(loc),
            "volatile" => KwdVolatile(loc),
            "while" => KwdWhile(loc),
            _ => Identifier(loc, string),
        })
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
            c = next_char!(fatal self, &loc);
            if c == 'x' || c == 'X' {
                use_hex = true;
                c = next_char!(fatal self, &loc);
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
                        None => return Some(IntLit(loc, int_value, int_width)),
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
                        None => return Some(IntLit(loc, int_value, int_width)),
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
                    None => return Some(IntLit(loc, int_value, int_width)),
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
                        None => return Some(FloatLit(loc, float_value, float_width)),
                    };
                }
            }
            else {
                while c.is_ascii_digit() {
                    decimal /= 10.;
                    float_value += decimal * (c as i32 - '0' as i32) as f64;
                    c = match self.next_char() {
                        Some(c) => c,
                        None => return Some(FloatLit(loc, float_value, float_width)),
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

            c = next_char!(fatal self, &loc);
            if c == '+' {
                c = next_char!(fatal self, &loc);
            }
            else if c == '-' {
                neg_exponent = true;
                c = next_char!(fatal self, &loc);
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
                return Some(FloatLit(loc, float_value, float_width));
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
                    self.save_char();
                    break;
                },
            }
            c = self.next_char()?;
        }
        
        if use_float {
            Some(FloatLit(loc, float_value, float_width))
        }
        else {
            Some(IntLit(loc, int_value, int_width))
        }
    }

    fn scan_char(&mut self) -> Option<Token> {
        let loc = self.loc.clone();
        let (mut c, mut escape) = self.next_char_esc()?;
        if c == '\'' && !escape {
            self.error(&loc, Error::EmptyCharLit);
            return Some(CharLit(loc, '\0'));
        }

        let value = c;
        (c, escape) = self.next_char_esc()?;
        if c == '\'' && !escape {
            return Some(CharLit(loc, value));
        }

        self.warning(&loc, Error::MulticharLiteral);
        let mut int_value = value as u64;
        while c != '\'' || escape {
            int_value <<= 8;
            int_value |= c as u64;
            (c, escape) = self.next_char_esc()?;
        }
        Some(IntLit(loc, int_value, NumberSize::Normal))
    }

    fn scan_string(&mut self) -> Option<Token> {
        let loc = self.loc.clone();
        let (mut c, mut escape) = self.next_char_esc()?;
        let string = String::new();
        // TODO Finish implementing this
        None
    }
}