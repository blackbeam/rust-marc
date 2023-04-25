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
    Utf8Error(std::str::Utf8Error),
    #[cfg(feature = "xml")]
    XmlError(xml::writer::Error),
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
            Error::Utf8Error(err) => write!(f, "UTF8 error: {}", err),
            #[cfg(feature = "xml")]
            Error::XmlError(err) => write!(f, "XML error: {}", err),
        }
    }
}

impl ::std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::UnexpectedByteInDecNum(_)
            | Error::FieldTooLarge(_)
            | Error::RecordTooLarge(_)
            | Error::RecordTooShort(_)
            | Error::UnexpectedEofInDecNum
            | Error::UnexpectedEof
            | Error::UnexpectedEofInDirectory
            | Error::NoRecordTerminator
            | Error::UnexpectedSubfieldEnd
            | Error::NonUnicodeSequence(_)
            | Error::UnknownCharacterCodingScheme(_)
            | Error::Io(_) => None,
            Error::Utf8Error(ref e) => Some(e as &_),
            #[cfg(feature = "xml")]
            Error::XmlError(ref e) => Some(e as &_),
        }
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::Io(err.kind())
    }
}

impl From<std::str::Utf8Error> for Error {
    fn from(err: std::str::Utf8Error) -> Error {
        Error::Utf8Error(err)
    }
}
