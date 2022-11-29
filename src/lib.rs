use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io;
use std::io::Read;
use crate::errors::ErrorFunc;
use crate::lex::*;

mod errors;
mod lex;

#[derive(PartialOrd, PartialEq)]
pub enum Standard {
    C89 = 1989,
    C99 = 1999,
    C11 = 2011,
    C17 = 2017,
    C23 = 2023,
}

#[derive(Debug, Clone)]
pub struct Location {
    pub file_name: String,
    pub line: usize,
    pub column: usize,
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}.{}", self.file_name, self.line, self.column)
    }
}

pub struct Context {
    input: Vec<char>,
    position: usize,
    loc: Location,
    standard: Standard,
    tokens: Vec<Token>,
    warning_func: Option<ErrorFunc>,
    error_func: Option<ErrorFunc>,
}

impl Context {
    pub fn new(input: String, file_name: String, standard: Standard) -> Self {
        Self {
            input: input.chars().collect(),
            position: 0,
            loc: Location {
                file_name,
                line: 1,
                column: 0,
            },
            standard,
            tokens: Vec::new(),
            warning_func: None,
            error_func: None,
        }
    }

    pub fn from_file(file_path: String, standard: Standard) -> Result<Self, io::Error> {
        let mut f = File::open(&file_path)?;
        let mut buffer = String::new();
        f.read_to_string(&mut buffer)?;
        Ok(Self {
            input: buffer.chars().collect(),
            position: 0,
            loc: Location {
                file_name: file_path,
                line: 1,
                column: 0,
            },
            standard,
            tokens: Vec::new(),
            warning_func: None,
            error_func: None,
        })
    }
}

#[cfg(test)]
mod tests;