use std::{fmt, io};

use crate::tag::Tag;

pub type Result<T> = ::std::result::Result<T, Error>;

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
    Utf8Error(String),
    #[cfg(feature = "xml")]
    XmlError(String),
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
            Error::Io(err) => write!(f, "IO error: {:?}", err),
            Error::Utf8Error(err) => write!(f, "UTF8 error: {}", err),
            #[cfg(feature = "xml")]
            Error::XmlError(err) => write!(f, "XML error: {}", err),
        }
    }
}

impl ::std::error::Error for Error {}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::Io(err.kind())
    }
}

impl From<std::str::Utf8Error> for Error {
    fn from(err: std::str::Utf8Error) -> Error {
        Error::Utf8Error(err.to_string())
    }
}
