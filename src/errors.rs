use crate::*;

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
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Error::BadEscapeSequence => "invalid string escape sequence",
            Error::MulticharLiteral => "multi-character literal",
            Error::BadToken => "unexpected token",
            Error::BadIntLitSuffix => "invalid suffix for integer literal",
            Error::BadFloatLitSuffix => "invalid suffix for floating-point literal",
            Error::BadExponent => "invalid exponent",
            Error::BadOctalDigit => "invalid digit in octal integer literal",
            Error::EmptyCharLit => "empty character literal",
            Error::UnexpectedEOF => "unexpected end of input",
            Error::NewlineInString => "newline in string literal",
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