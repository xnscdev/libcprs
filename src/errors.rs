use crate::*;
use crate::errors::Error::*;

pub enum Error {
    BadEscapeSequence,
    MulticharLiteral,
    BadToken,
    BadIntLitSuffix,
    BadFloatLitSuffix,
    BadExponent,
    BadOctalDigit,
    EmptyCharLit,
    UnexpectedEOF,
    NewlineInString,
    UnterminatedComment,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            BadEscapeSequence => "invalid string escape sequence",
            MulticharLiteral => "multi-character literal",
            BadToken => "unexpected token",
            BadIntLitSuffix => "invalid suffix for integer literal",
            BadFloatLitSuffix => "invalid suffix for floating-point literal",
            BadExponent => "invalid exponent",
            BadOctalDigit => "invalid digit in octal integer literal",
            EmptyCharLit => "empty character literal",
            UnexpectedEOF => "unexpected end of input",
            NewlineInString => "newline in string literal",
            UnterminatedComment => "unterminated comment",
        })
    }
}

pub type ErrorFunc = fn(&Location, Error);

impl Context {
    pub fn set_warning_func(&mut self, warning_func: ErrorFunc) {
        self.warning_func = Some(warning_func);
    }

    pub fn set_error_func(&mut self, error_func: ErrorFunc) {
        self.error_func = Some(error_func);
    }

    pub(crate) fn warning(&self, loc: &Location, err: Error) {
        match self.warning_func {
            Some(f) => f(loc, err),
            None => {},
        }
    }

    pub(crate) fn error(&self, loc: &Location, err: Error) {
        match self.error_func {
            Some(f) => f(loc, err),
            None => {},
        }
    }
}