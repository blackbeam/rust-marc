use std::{fmt, io};

use crate::{tag::Tag, Identifier};

pub type Result<T, E = Error> = ::std::result::Result<T, E>;

/// Points to a record data.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Pointer {
    /// Points to a record leader.
    Leader,
    /// Points to a filed.
    Field(Tag),
    /// Points to a subfield.
    Subfield(Tag, Identifier),
}

/// Errors of this crate.
#[derive(Debug)]
pub enum Error {
    UnexpectedByteInDecNum(u8),
    FieldTooLarge(Tag),
    RecordTooLarge(usize),
    RecordTooShort(usize),
    UnexpectedEofInDecNum,
    UnexpectedEof,
    UnexpectedEofInDirectory,
    NoRecordTerminator,
    UnexpectedSubfieldEnd,
    NonUnicodeSequence(Pointer),
    UnknownCharacterCodingScheme(u8),
    Io(io::ErrorKind),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::UnexpectedByteInDecNum(byte) => {
                write!(f, "Unexpected byte in decimal string: {}", byte)
            }
            Error::FieldTooLarge(tag) => {
                write!(f, "Field byte length is greater than 9998 (tag={})", tag)
            }
            Error::RecordTooLarge(size) => {
                write!(f, "Record byte length {} is greater than 99999 limit", size)
            }
            Error::RecordTooShort(len) => {
                write!(f, "Record length {} specified in leader is too small", len)
            }
            Error::UnexpectedEofInDecNum => {
                write!(f, "Unexpected EOF while reading decimal number")
            }
            Error::UnexpectedEof => write!(f, "Unexpected EOF"),
            Error::UnexpectedEofInDirectory => write!(f, "Unexpected EOF while reading directory"),
            Error::NoRecordTerminator => write!(f, "No record terminator"),
            Error::UnexpectedSubfieldEnd => write!(f, "Unexpected end of a subfield"),
            Error::UnknownCharacterCodingScheme(val) => {
                write!(f, "Unknown character coding scheme 0x{val:02x}")
            }
            Error::NonUnicodeSequence(ptr) => match ptr {
                Pointer::Leader => write!(f, "Non unicode sequence in the record leater"),
                Pointer::Field(tag) => write!(f, "Non unicode sequence in field {}", tag),
                Pointer::Subfield(tag, id) => {
                    write!(f, "Non unicode sequence in subfield {}${}", tag, id)
                }
            },
            Error::Io(err) => write!(f, "IO error: {:?}", err),
        }
    }
}

impl ::std::error::Error for Error {}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::Io(err.kind())
    }
}
