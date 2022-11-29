use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io;
use std::io::Read;

mod lex;

pub enum Standard {
    C89,
    C99,
    C11,
    C17,
    C23,
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
    input: String,
    position: usize,
    loc: Location,
    errors: usize,
    indent: usize,
    standard: Standard,
}

impl Context {
    pub fn new(input: String, file_name: String, standard: Standard) -> Self {
        Self {
            input,
            position: 0,
            loc: Location {
                file_name,
                line: 1,
                column: 0,
            },
            errors: 0,
            indent: 0,
            standard,
        }
    }

    pub fn from_file(file_path: String, standard: Standard) -> Result<Self, io::Error> {
        let mut f = File::open(&file_path)?;
        let mut buffer = String::new();
        f.read_to_string(&mut buffer)?;
        Ok(Self {
            input: buffer,
            position: 0,
            loc: Location {
                file_name: file_path,
                line: 1,
                column: 0,
            },
            errors: 0,
            indent: 0,
            standard,
        })
    }
}

#[cfg(test)]
mod tests;