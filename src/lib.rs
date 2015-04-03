#![feature(io, collections, unicode, convert)]
#![cfg_attr(test, feature(test))]

#[cfg(test)] extern crate test;

use std::borrow::Borrow;
use std::borrow::ToOwned;
use std::char;
use std::io;
use std::io::Read;
use std::io::Error;
use std::io::ErrorKind::Other;
use std::slice;
use std::str;

const MIN_REC_LEN: u32 = 24;
const MAX_REC_LEN: u32 = 99_999;
const RECORD_TERMINATOR: u8 = 0x1D;
const FIELD_TERMINATOR: u8 = 0x1E;
const SUBFIELD_DELIMITER: u8 = 0x1F;

/// Field representation.
pub type FieldRepr = (Tag, Vec<u8>);

/// Subfield representation.
pub type SubfieldRepr = (Identifier, Vec<u8>);

/// Field tag representation.
///
/// `Into<Tag>` are implemented for `&[u8]`, `&str`, `[u8; 3]` and `Tag`.
///
/// `From<&Tag>` are implemented for `&[u8]`, `&str`, `[u8; 3]` and `&Tag`.
#[derive(Eq, PartialEq, Ord, PartialOrd,
         Clone, Hash, Debug)]
pub struct Tag([u8; 3]);

/// Data field indicator representation.
#[derive(Eq, PartialEq, Ord, PartialOrd,
         Clone, Hash, Debug)]
pub struct Indicator([u8; 2]);

/// Subfield identifier representation.
#[derive(Eq, PartialEq, Ord, PartialOrd,
         Clone, Copy, Hash, Debug)]
pub struct Identifier(u8);

/// Reference to a data of a field.
pub struct FieldData<'a>(&'a [u8]);

// -> (tag, field_len, start_pos)
type DirectoryEntry = (Tag, u32, u32);

impl Borrow<[u8]> for Tag {
    fn borrow(&self) -> &[u8] {
        let &Tag(ref tag) = self;
        tag
    }
}

impl<'a> Borrow<[u8]> for FieldData<'a> {
    fn borrow(&self) -> &[u8] {
        let &FieldData(ref data) = self;
        data
    }
}

impl<'a> From<&'a [u8]> for Tag {
    fn from(s: &'a [u8]) -> Tag {
        assert!(s.len() == 3, "Tag length != 3");
        Tag([s[0], s[1], s[2]])
    }
}

impl<'a> From<&'a str> for Tag {
    fn from(s: &'a str) -> Tag {
        s.as_bytes().into()
    }
}

impl From<[u8; 3]> for Tag {
    fn from(s: [u8; 3]) -> Tag {
        Tag(s)
    }
}

impl<'a> From<&'a Tag> for &'a str {
    fn from(tag: &'a Tag) -> &'a str {
        str::from_utf8(tag.borrow()).unwrap()
    }
}

impl<'a> From<&'a Tag> for &'a [u8] {
    fn from(tag: &'a Tag) -> &'a [u8] {
        tag.borrow()
    }
}

impl<'a> From<&'a Tag> for [u8; 3] {
    fn from(tag: &'a Tag) -> [u8; 3] {
        let Tag(x) = tag.clone();
        x
    }
}

/// Entities that can be created from data of a field or a subfield.
pub trait FromFieldData {
    fn from_data(data: &[u8]) -> Option<&Self>;
}

impl FromFieldData for [u8] {
    #[inline]
    fn from_data(data: &[u8]) -> Option<&[u8]> {
        Some(data)
    }
}

impl FromFieldData for str {
    #[inline]
    fn from_data(data: &[u8]) -> Option<&str> {
        str::from_utf8(data).ok()
    }
}

impl From<Identifier> for u8 {
    fn from(Identifier(x): Identifier) -> u8 {
        x
    }
}

impl From<Identifier> for char {
    fn from(Identifier(x): Identifier) -> char {
        char::from_u32(x as u32).unwrap()
    }
}

impl From<u8> for Identifier {
    fn from(x: u8) -> Identifier {
        Identifier(x)
    }
}

impl From<char> for Identifier {
    fn from(x: char) -> Identifier {
        assert_eq!(x.len_utf8(), 1);
        let mut dst = [0u8];
        x.encode_utf8(&mut dst);
        Identifier(dst[0])
    }
}

impl Borrow<[u8]> for Indicator {
    fn borrow(&self) -> &[u8] {
        let &Indicator(ref ind) = self;
        ind
    }
}

impl From<Indicator> for [u8; 2] {
    fn from(Indicator(ind): Indicator) -> [u8; 2] {
        ind
    }
}

impl From<Indicator> for [char; 2] {
    fn from(Indicator(ind): Indicator) -> [char; 2] {
        [char::from_u32(ind[0] as u32).unwrap(), char::from_u32(ind[1] as u32).unwrap()]
    }
}

impl From<[char; 2]> for Indicator {
    fn from(bs: [char; 2]) -> Indicator {
        assert_eq!(bs[0].len_utf8(), 1);
        assert_eq!(bs[1].len_utf8(), 1);
        let mut dst = [0u8; 2];
        bs[0].encode_utf8(&mut dst);
        bs[1].encode_utf8(&mut dst[1..]);
        Indicator(dst)
    }
}

impl From<[u8; 2]> for Indicator {
    fn from(bs: [u8; 2]) -> Indicator {
        Indicator(bs)
    }
}

impl<'a> From<Field<'a>> for FieldRepr {
    fn from(Field {tag, data}: Field<'a>) -> FieldRepr {
        (tag, data.to_owned())
    }
}

impl<'a> From<Subfield<'a>> for SubfieldRepr {
    fn from(sf: Subfield<'a>) -> SubfieldRepr {
        (sf.ident, sf.data.to_owned())
    }
}

trait LeaderField {
    fn from_byte(byte: u8) -> Self;
    fn to_byte(&self) -> u8;
}

macro_rules! read_exact(
    ($source:expr, $dest:expr, 1, $edesc:expr) => ({
        match $source.read(&mut $dest) {
            Ok(c) => {
                if c == 1 {
                    Ok(())
                } else {
                    Err(Error::new(Other, $edesc))
                }
            },
            Err(e) => Err(e),
        }
    });
    ($source:expr, $dest:expr, $count:expr, $edesc:expr) => ({
        let mut rc = $count;
        let mut err = None;
        loop {
            match $source.read(&mut $dest[($count - rc)..rc]) {
                Ok(c) => {
                    if c == rc {
                        break;
                    } else if c == 0 {
                        err = Some(Error::new(Other, $edesc));
                        break
                    }
                    rc -= c;
                },
                Err(e) => {
                    err = Some(e);
                    break;
                }
            }
        }
        if err.is_some() {
            Err(err.unwrap())
        } else {
            Ok(())
        }
    });
);

trait MrcWriteInternal: io::Write {
    fn write_dec_num(&mut self, len: u8, mut value: u32) -> io::Result<()> {
        let mut buf = [0x30u8; 10];
        for i in 0u8..len {
            buf[len as usize - i as usize - 1] = (value % 10) as u8 + 0x30;
            value = value / 10;
            if value == 0 {
                break;
            }
        }
        self.write_all(&(&buf)[0..len as usize])
    }
}

impl<T: io::Write + ?Sized> MrcWriteInternal for T {}

trait MrcReadInternal: io::Read {
    #[inline]
    fn read_leader_field<T: LeaderField>(&mut self) -> io::Result<T> {
        let mut buf = [0u8];
        try!(read_exact!(self, buf, 1, "Unexpected EOF while reading leader field"));
        Ok(T::from_byte(buf[0]))
    }
    fn read_dec_num(&mut self, len: u8) -> io::Result<u32> {
        assert!(0 < len && len < 11);
        let mut buf = [0u8; 11];
        try!(read_exact!(self, buf, len as usize, "Unexpected EOF while reading decimal number"));
        let mut res = 0u32;
        let mut pos = len;
        for &i in &buf[..(len as usize)] {
            if b'0' > i || i > b'9' {
                return Err(Error::new(Other, "Unexpected byte while reading decimal number"));
            }
            let mut x = (i - 0x30) as u32;
            if pos > 0 {
                if x != 0 {
                    for _ in 1..pos {
                        x *= 10
                    }
                }
                pos -= 1;
            }
            res += x;
        }
        Ok(res)
    }
    // -> (tag, field_len, start_pos)
    fn read_directory_entry(&mut self) -> io::Result<DirectoryEntry> {
        let mut tag = [0u8; 3];
        try!(read_exact!(self, tag, 3, "Unexpected EOF while reading directory entry"));
        let field_len = try!(self.read_dec_num(4));
        let start_pos = try!(self.read_dec_num(5));
        Ok((Tag(tag), field_len, start_pos))
    }
}

impl<T: io::Read + ?Sized> MrcReadInternal for T {}

/// io::Read extension which provides `read_record` method.
pub trait MrcRead: io::Read {
    fn read_record(&mut self) -> io::Result<Option<Record>> {
        let mut rec = io::Cursor::new(Vec::with_capacity(5));

        match self.take(5).read_to_end(rec.get_mut()) {
            Ok(0) => return Ok(None),
            Ok(x) if x < 5 => {
                return Err(Error::new(Other, "Unexpected EOF while reading record length"))
            },
            Err(e) => return Err(e),
            _ => (),
        }

        let record_length = try!(rec.read_dec_num(5));
        if MIN_REC_LEN > record_length || record_length > MAX_REC_LEN {
            return Err(Error::new(Other, "Record length is out of bounds"));
        }

        rec.get_mut().reserve(record_length as usize - 5);
        match self.take(record_length as u64 - 5).read_to_end(rec.get_mut()) {
            Ok(x) if x < record_length as usize - 5 => {
                return Err(Error::new(Other, "Unexpected EOF while reading record"));
            },
            Err(e) => return Err(e),
            _ => (),
        }
        if rec.get_ref()[rec.get_ref().len()-1] != RECORD_TERMINATOR {
            return Err(Error::new(Other, "No record terminator"));
        }

        let record = try!(Record::from_data(rec.into_inner()));
        Ok(Some(record))
    }
}

impl<T: io::Read + ?Sized> MrcRead for T {}

#[derive(Debug, PartialEq, Clone)]
pub enum Warning {
    WrongIndicatorCount(u8),
    WrongSubfieldCodeCount(u8),
    WrongLengthOfFieldLen(u8),
    WrongStartingCharacterPositionLen(u8),
    WrongImplementationDefinedPortionLen(u8),
}

/// Iterator over fields of a record.
#[derive(Clone)]
pub struct Fields<'a> {
    record: &'a Record,
    vec_iter: slice::Iter<'a, DirectoryEntry>,
    remain: u32,
}

impl<'a> Iterator for Fields<'a> {
    type Item = Field<'a>;

    fn next(&mut self) -> Option<Field<'a>> {
        if let Some(entry) = self.vec_iter.next() {
            self.remain -= 1;
            Some(Field::from_entry(self.record, entry))
        } else {
            None
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remain as usize, Some(self.remain as usize))
    }
}

#[derive(Debug, PartialEq, Clone)]
/// Borrowed version of a field of a record.
pub struct Field<'a> {
    data: &'a [u8],
    tag: Tag,
}

impl<'a> Field<'a> {
    fn from_entry<'r>(rec: &'r Record, entry: &DirectoryEntry) -> Field<'r> {
        let begin = (rec.base_address_of_data + entry.2) as usize;
        let end = begin + entry.1 as usize;
        Field {
            data: &rec.data[begin..end],
            tag: entry.0.clone(),
        }
    }

    /// Returns true if `self` is data field.
    #[inline]
    pub fn is_data_field(&self) -> bool {
        self.data[2] == SUBFIELD_DELIMITER
    }

    /// Returns true if `self` is control field.
    #[inline]
    pub fn is_control_field(&self) -> bool {
        ! self.is_data_field()
    }

    /// Returns subfields with identifier `ident` or empty vec if no such subfields.
    pub fn get_subfield<'f, T: Into<Identifier>>(&'f self, ident: T) -> Vec<Subfield<'f>> {
        let mut res = Vec::new();
        if self.is_data_field() && self.data.len() > 2 {
            let ident: Identifier = ident.into();
            let mut start = None;
            for (i, &x) in self.data.iter().enumerate() {
                if i < 2 {
                    continue;
                }
                if start.is_none() {
                    if self.data[i-1] == SUBFIELD_DELIMITER {
                        if x == ident.into() {
                            start = Some(i - 1);
                        }
                    }
                } else {
                    if x == SUBFIELD_DELIMITER || x == FIELD_TERMINATOR {
                        res.push(Subfield {
                            data: &self.data[start.unwrap()..i],
                            tag: self.tag.clone(),
                            ident: ident,
                        });
                        start = None
                    }
                }
            }
        }
        res
    }

    /// Returns iterator over subfields of a data field.
    #[inline]
    pub fn subfields<'f>(&'f self) -> Subfields<'f> {
        Subfields::new(self.clone())
    }

    /// Returns indictor of data field or `None` if self is control field.
    #[inline]
    pub fn get_indicator<T: From<Indicator>>(&self) -> Option<T> {
        if self.is_data_field() {
            Some(From::from([self.data[0], self.data[1]].into()))
        } else {
            None
        }
    }

    /// Returns data of a control field or `None` if self is data field.
    #[inline]
    pub fn get_data<T: FromFieldData + ?Sized>(&self) -> Option<&T> {
        if ! self.is_data_field() && self.data.len() > 1 {
            FromFieldData::from_data(&self.data[0..(self.data.len()-1)])
        } else {
            None
        }
    }

    /// Returns tag of a field.
    #[inline]
    pub fn get_tag<T: From<&'a Tag>>(&'a self) -> T {
        From::from(&self.tag)
    }
}

/// Iterator over subfields of a data field.
#[derive(Clone)]
pub struct Subfields<'a> {
    field: Field<'a>,
    offset: usize,
    start: Option<usize>,
    ident: Option<Identifier>,
    remain: u32,
}

impl<'a> Subfields<'a> {
    fn new(field: Field<'a>) -> Subfields<'a> {
        let mut subfields = Subfields {
            field: field,
            offset: 0,
            start: None,
            ident: None,
            remain: 0,
        };
        subfields.count_subfields();
        subfields
    }

    fn count_subfields(&mut self) {
        for &x in self.field.data.iter() {
            if x == SUBFIELD_DELIMITER {
                self.remain += 1;
            }
        }
    }
}

impl<'a> Iterator for Subfields<'a> {
    type Item = Subfield<'a>;

    fn next(&mut self) -> Option<Subfield<'a>> {
        if self.field.is_control_field() || self.field.data[self.offset] == FIELD_TERMINATOR {
            None
        } else if self.start.is_none() {
            for (i, &x) in (&self.field.data[self.offset..]).iter().enumerate() {
                if (self.offset + i) < 2 {
                    continue;
                }
                if x == SUBFIELD_DELIMITER {
                    self.start = Some(self.offset+i);
                    self.ident = Some(self.field.data[self.offset+i+1].into());
                    self.offset += i + 1;
                    break;
                } else if x == FIELD_TERMINATOR {
                    return None;
                }
            }
            self.next()
        } else {
            let mut subfield = None;
            let start = self.start.take().unwrap();
            for (i, &x) in (&self.field.data[self.offset..]).iter().enumerate() {
                if x == SUBFIELD_DELIMITER || x == FIELD_TERMINATOR {
                    self.offset += i - 1;
                    self.remain -= 1;
                    subfield = Some(Subfield {
                        data: &self.field.data[start..start+i+1],
                        tag: self.field.tag.clone(),
                        ident: self.ident.take().unwrap(),
                    });
                    break;
                }
            }
            subfield
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remain as usize, Some(self.remain as usize))
    }
}

#[derive(Debug, PartialEq, Clone)]
/// Borrowed version of a subfield of a data field.
pub struct Subfield<'a> {
    data: &'a [u8],
    tag: Tag,
    ident: Identifier,
}

impl<'a> Subfield<'a> {
    /// Returns identifier of a subfield.
    #[inline]
    pub fn get_identifier<T: From<Identifier>>(&self) -> T {
        From::from(self.ident)
    }

    /// Returns data of a subfield.
    #[inline]
    pub fn get_data<T: FromFieldData + ?Sized>(&self) -> Option<&T> {
        if self.data.len() > 2 {
            FromFieldData::from_data(&self.data[2..self.data.len()])
        } else {
            None
        }
    }

    /// Returns tag of a field `self` belongs to.
    #[inline]
    pub fn get_tag<T: From<&'a Tag>>(&'a self) -> T {
        From::from(&self.tag)
    }
}

/// Allows you to build MARC variable data fields.
pub struct DataFieldBuilder {
    tag: Tag,
    indicator: Indicator,
    subfields: Vec<SubfieldRepr>,
    size: u32,
}

impl DataFieldBuilder {
    pub fn new<T, I, S>(tag: T, indicator: I, subfield: S) -> DataFieldBuilder
    where T: Into<Tag> + Clone, I: Into<Indicator>, S: Into<SubfieldRepr> {
        let tag = tag.clone().into();
        let indicator = indicator.into();
        let subfield = subfield.into();
        let mut res = DataFieldBuilder {
            tag: tag,
            indicator: indicator,
            subfields: Vec::new(),
            size: 2 + subfield.1.len() as u32 + 1,
        };
        res.subfields.insert(0, subfield);
        res
    }
    pub fn add_subfield<T: Into<SubfieldRepr>>(mut self, subfield: T) -> DataFieldBuilder {
        let subfield = subfield.into();
        self.size += subfield.1.len() as u32;
        let mut index = 0;
        for (i, sf) in self.subfields.iter().enumerate() {
            if sf.0 >= subfield.0 {
                index = i;
                break;
            }
        }
        self.subfields.insert(index, subfield);
        self
    }
    /// Removes all subfields with identifier `ident`.
    pub fn remove_subfield<T: Into<Identifier>>(mut self, ident: T) -> DataFieldBuilder {
        let ident = ident.into();
        self.subfields.retain(|subfield| {
            subfield.0 != ident
        });
        self
    }
    pub fn into_field_repr(self) -> FieldRepr {
        let tag = self.tag;
        let mut data = Vec::with_capacity(self.size as usize);
        data.push_all(self.indicator.borrow());
        for (ident, mut subfiled_data) in self.subfields.into_iter() {
            data.push(ident.into());
            data.append(&mut subfiled_data);
        }
        data.push(FIELD_TERMINATOR);
        (tag, data)
    }
}

/// Allows you to build MARC records.
pub struct RecordBuilder {
    record_status: RecordStatus,
    type_of_record: TypeOfRecord,
    bibliographic_level: BibliographicLevel,
    type_of_control: TypeOfControl,
    character_coding_scheme: CharacterCodingScheme,
    encoding_level: EncodingLevel,
    descriptive_cataloging_form: DescriptiveCatalogingForm,
    multipart_resource_record_level: MultipartResourceRecordLevel,
    fields: Vec<FieldRepr>,
    size: u32,
}

impl RecordBuilder {
    pub fn new() -> RecordBuilder {
        RecordBuilder {
            record_status: RecordStatus::IncreaseInEncodingLevel,
            type_of_record: TypeOfRecord::LanguageMaterial,
            bibliographic_level: BibliographicLevel::MonographicComponentPart,
            type_of_control: TypeOfControl::NoSpecifiedType,
            character_coding_scheme: CharacterCodingScheme::Marc8,
            encoding_level: EncodingLevel::FullLevel,
            descriptive_cataloging_form: DescriptiveCatalogingForm::NonIsbd,
            multipart_resource_record_level: MultipartResourceRecordLevel::NotSpecifiedOrNotApplicable,
            fields: Vec::new(),
            size: 24,
        }
    }
    pub fn from_record(rec: &Record) -> RecordBuilder {
        let mut res = RecordBuilder {
            record_status: rec.record_status.clone(),
            type_of_record: rec.type_of_record.clone(),
            bibliographic_level: rec.bibliographic_level.clone(),
            type_of_control: rec.type_of_control.clone(),
            character_coding_scheme: rec.character_coding_scheme.clone(),
            encoding_level: rec.encoding_level.clone(),
            descriptive_cataloging_form: rec.descriptive_cataloging_form.clone(),
            multipart_resource_record_level: rec.multipart_resource_record_level.clone(),
            fields: Vec::new(),
            size: 24,
        };
        for field in rec.fields() {
            res = res.add_field(field);
        }
        res
    }
    #[inline]
    pub fn set_record_status(mut self, x: RecordStatus) -> RecordBuilder {
        self.record_status = x; self
    }
    #[inline]
    pub fn set_type_of_record(mut self, x: TypeOfRecord) -> RecordBuilder {
        self.type_of_record = x; self
    }
    #[inline]
    pub fn set_bibliographic_level(mut self, x: BibliographicLevel) -> RecordBuilder {
        self.bibliographic_level = x; self
    }
    #[inline]
    pub fn set_type_of_control(mut self, x: TypeOfControl) -> RecordBuilder {
        self.type_of_control = x; self
    }
    #[inline]
    pub fn set_character_coding_scheme(mut self, x: CharacterCodingScheme) -> RecordBuilder {
        self.character_coding_scheme = x; self
    }
    #[inline]
    pub fn set_encoding_level(mut self, x: EncodingLevel) -> RecordBuilder {
        self.encoding_level = x; self
    }
    #[inline]
    pub fn set_descriptive_cataloging_form(mut self, x: DescriptiveCatalogingForm) -> RecordBuilder {
        self.descriptive_cataloging_form = x; self
    }
    #[inline]
    pub fn set_multipart_resource_record_level(mut self, x: MultipartResourceRecordLevel) -> RecordBuilder {
        self.multipart_resource_record_level = x; self
    }
    pub fn add_field<T: Into<FieldRepr>>(mut self, def: T) -> RecordBuilder {
        let repr = def.into();
        let mut index = 0;
        self.size += repr.1.len() as u32 + 12;
        for (i, f) in self.fields.iter().enumerate() {
            if f.0 >= repr.0 {
                index = i;
                break;
            }
        }
        self.fields.insert(index, repr);
        self
    }
    /// Removes all fields with tag `tag`.
    pub fn remove_field<T: Into<Tag> + Clone>(mut self, tag: T) -> RecordBuilder {
        let tag = tag.into();
        self.fields.retain(|field| {
            field.0 != tag
        });
        self
    }
    pub fn into_record(self) -> Record {
        let mut data = Vec::with_capacity(self.size as usize);
        let _ = data.write_dec_num(5, self.size);
        let _ = io::Write::write_all(&mut data, &[
            self.record_status.to_byte(),
            self.type_of_record.to_byte(),
            self.bibliographic_level.to_byte(),
            self.type_of_control.to_byte(),
            self.character_coding_scheme.to_byte(),
            b'2',
            b'2',
        ]);
        let _ = data.write_dec_num(5, 24 + 12 * self.fields.len() as u32 + 1);
        let _ = io::Write::write_all(&mut data, &[
            self.encoding_level.to_byte(),
            self.descriptive_cataloging_form.to_byte(),
            self.multipart_resource_record_level.to_byte(),
            b'4',
            b'5',
            b'0',
            b'0',
        ]);
        let mut offset = 0;
        for field_repr in self.fields.iter() {
            let _ = io::Write::write_all(&mut data, field_repr.0.borrow());
            let _ = data.write_dec_num(4, field_repr.1.len() as u32);
            let _ = data.write_dec_num(5, offset);
            offset += field_repr.1.len() as u32;
        }
        data.push(FIELD_TERMINATOR);
        for (_, mut field_data) in self.fields.into_iter() {
            data.append(&mut field_data);
        }
        data.push(RECORD_TERMINATOR);
        Record::from_data(data).unwrap()
    }
}

#[derive(Debug, PartialEq, Clone)]
/// Parsed MARC record representation.
pub struct Record {
    length: u32,
    base_address_of_data: u32,
    indicator_count: u8,
    subfield_code_count: u8,
    length_of_field_len: u8,
    starting_character_position_len: u8,
    implementation_defined_portion_len: u8,
    pub record_status: RecordStatus,
    pub type_of_record: TypeOfRecord,
    pub bibliographic_level: BibliographicLevel,
    pub type_of_control: TypeOfControl,
    pub character_coding_scheme: CharacterCodingScheme,
    pub encoding_level: EncodingLevel,
    pub descriptive_cataloging_form: DescriptiveCatalogingForm,
    pub multipart_resource_record_level: MultipartResourceRecordLevel,
    warnings: Vec<Warning>,
    data: Vec<u8>,
    directory_entryes: Option<Vec<DirectoryEntry>>,
}

impl Record {
    pub fn from_vec(data: Vec<u8>) -> io::Result<Record> {
        let record_length = try!((&data[..]).read_dec_num(5));
        if record_length as usize != data.len() {
            return Err(Error::new(Other, "Vector length does not equals record length"));
        }

        if data[data.len() - 1] != RECORD_TERMINATOR {
            return Err(Error::new(Other, "Record does not ends with record terminator"));
        }

        Record::from_data(data)
    }
    fn from_data(data: Vec<u8>) -> io::Result<Record> {
        let mut rec = io::Cursor::new(data);
        let record_length = try!(rec.read_dec_num(5));
        let mut warnings = Vec::new();
        let record_status = try!(rec.read_leader_field());
        let type_of_record = try!(rec.read_leader_field());
        let bibliographic_level = try!(rec.read_leader_field());
        let type_of_control = try!(rec.read_leader_field());
        let character_coding_scheme = try!(rec.read_leader_field());
        let indicator_count = try!(rec.read_dec_num(1)) as u8;
        if indicator_count != 2 {
            warnings.push(Warning::WrongIndicatorCount(indicator_count));
        }
        let subfield_code_count = try!(rec.read_dec_num(1)) as u8;
        if subfield_code_count != 2 {
            warnings.push(Warning::WrongSubfieldCodeCount(subfield_code_count));
        }
        let base_address_of_data = try!(rec.read_dec_num(5));
        let encoding_level = try!(rec.read_leader_field());
        let descriptive_cataloging_form = try!(rec.read_leader_field());
        let multipart_resource_record_level = try!(rec.read_leader_field());
        let length_of_field_len = try!(rec.read_dec_num(1)) as u8;
        if length_of_field_len != 4 {
            warnings.push(Warning::WrongLengthOfFieldLen(length_of_field_len));
        }
        let starting_character_position_len = try!(rec.read_dec_num(1)) as u8;
        if starting_character_position_len != 5 {
            warnings.push(
                Warning::WrongStartingCharacterPositionLen(starting_character_position_len));
        }
        let implementation_defined_portion_len = try!(rec.read_dec_num(1)) as u8;
        if implementation_defined_portion_len != 0 {
            warnings.push(
                Warning::WrongImplementationDefinedPortionLen(implementation_defined_portion_len));
        }

        let mut record = Record {
            length: record_length,
            base_address_of_data: base_address_of_data,
            indicator_count: indicator_count,
            subfield_code_count: subfield_code_count,
            length_of_field_len: length_of_field_len,
            starting_character_position_len: starting_character_position_len,
            implementation_defined_portion_len: implementation_defined_portion_len,
            record_status: record_status,
            type_of_record: type_of_record,
            bibliographic_level: bibliographic_level,
            type_of_control: type_of_control,
            character_coding_scheme: character_coding_scheme,
            encoding_level: encoding_level,
            descriptive_cataloging_form: descriptive_cataloging_form,
            multipart_resource_record_level: multipart_resource_record_level,
            warnings: warnings,
            data: rec.into_inner(),
            directory_entryes: None,
        };
        try!(record.parse_directory());
        Ok(record)
    }

    /// Returns iterator over fields of a record.
    #[inline]
    pub fn fields<'r>(&'r self) -> Fields<'r> {
        Fields {
            record: self,
            vec_iter: self.directory_entryes.as_ref().unwrap().iter(),
            remain: self.directory_entryes.as_ref().unwrap().len() as u32,
        }
    }

    /// Returns fileds with tag `tag` or empty vec if no such fields.
    pub fn get_field<'r, T: Into<Tag> + Clone>(&'r self, tag: T) -> Vec<Field<'r>> {
        let tag = tag.into();
        let mut field = Vec::new();
        for x in self.directory_entryes.as_ref().unwrap().iter() {
            if x.0 == tag {
                field.push(Field::from_entry(self, x));
            }
        }
        field
    }

    /// Returns a slice into data of a field.
    #[inline]
    pub fn as_slice(&self) -> &[u8] {
        &self.data[..]
    }

    fn parse_directory(&mut self) -> io::Result<()> {
        let mut directory_entryes_count = 0;
        for i in 0..(self.data.len() - 24) {
            if self.data[i] == FIELD_TERMINATOR {
                directory_entryes_count = i / 12;
            }
        }
        let mut directory_entryes = Vec::with_capacity(directory_entryes_count);
        let mut dir = io::Cursor::new(& self.data[24..]);
        while dir.get_ref()[dir.position() as usize] != FIELD_TERMINATOR {
            directory_entryes.push(try!(dir.read_directory_entry()));
        }
        self.directory_entryes = Some(directory_entryes);
        Ok(())
    }
}

macro_rules! leader_field(
    ($name:ident {
        $($val:expr => $kind:ident,)+
    }) => (
        #[derive(Debug, PartialEq, Clone)]
        pub enum $name {
            $($kind),+,
            Unknown(u8),
        }

        impl LeaderField for $name {
            #[inline]
            fn from_byte(byte: u8) -> $name {
                match byte {
                    $($val => $name::$kind),+,
                    b => $name::Unknown(b),
                }
            }
            #[inline]
            fn to_byte(&self) -> u8 {
                match *self {
                    $($name::$kind => $val),+,
                    $name::Unknown(b) => b,
                }
            }
        }
    );
);

leader_field! {
    RecordStatus {
        b'a' => IncreaseInEncodingLevel,
        b'c' => CorrectedOrRevised,
        b'd' => Deleted,
        b'n' => New,
        b'p' => IncreaseInEncodingLevelFromPrepublication,
    }
}

leader_field! {
    TypeOfRecord {
        b'a' => LanguageMaterial,
        b'c' => NotatedMusic,
        b'd' => ManuscriptNotatedMusic,
        b'e' => CartographicMaterial,
        b'f' => ManuscriptCartographicMaterial,
        b'g' => ProjectedMedium,
        b'i' => NonmusicalSoundRecording,
        b'j' => MusicalSoundRecording,
        b'k' => TwoDimensionalNonprojectableGraphic,
        b'm' => ComputerFile,
        b'o' => Kit,
        b'p' => MixedMaterials,
        b'r' => ThreeDimensionalArtifactOrNaturallyOccurringObject,
        b't' => ManuscriptLanguageMaterial,
    }
}

leader_field! {
    BibliographicLevel {
        b'a' => MonographicComponentPart,
        b'b' => SerialComponentPart,
        b'c' => Collection,
        b'd' => Subunit,
        b'i' => IntegratingResource,
        b'm' => MonographOrItem,
        b's' => Serial,
    }
}

leader_field! {
    TypeOfControl {
        b' ' => NoSpecifiedType,
        b'a' => Archival,
    }
}

leader_field! {
    CharacterCodingScheme {
        b' ' => Marc8,
        b'a' => UcsUnicode,
    }
}

leader_field! {
    EncodingLevel {
        b' ' => FullLevel,
        b'1' => FullLevelMaterialNotExamined,
        b'2' => LessThanFullLevelMaterialNotExamined,
        b'3' => AbbreviatedLevel,
        b'4' => CoreLevel,
        b'5' => PartialLevel,
        b'7' => MinimalLevel,
        b'8' => PrepublicationLevel,
        b'u' => UnknownEL,
        b'z' => NotApplicable,
    }
}

leader_field! {
    DescriptiveCatalogingForm {
        b' ' => NonIsbd,
        b'a' => Aacr2,
        b'c' => IsbdPunctuationOmitted,
        b'i' => IsbdPunctuationIncluded,
        b'u' => UnknownDCF,
    }
}

leader_field! {
    MultipartResourceRecordLevel {
        b' ' => NotSpecifiedOrNotApplicable,
        b'a' => Set,
        b'b' => PartWithIndependentTitle,
        b'c' => PartWithDependentTitle,
    }
}

#[cfg(test)]
mod tests {
    const RECS: &'static str = "00963nam a2200229 i 4500001001000000003000800010003000800018005001700026008004100043035002300084040002600107041000800133072001900141100005800160245028000218260004000498300001600538650004200554856010400596979001200700979002100712\x1e000000001\x1eRuMoRGB\x1eEnMoRGB\x1e20080528120000.0\x1e080528s1992    ru a|||  a    |00 u rus d\x1e  \x1fa(RuMoEDL)-92k71098\x1e  \x1faRuMoRGB\x1fbrus\x1fcRuMoRGB\x1e0 \x1farus\x1e 7\x1fa07.00.03\x1f2nsnr\x1e1 \x1fa'Абд Ал-'Азиз Джа'фар Бин 'Акид\x1e00\x1faЭтносоциальная структура и институты социальной защиты в Хадрамауте (19 - первая половина 20 вв.) :\x1fbавтореферат дис. ... кандидата исторических наук : 07.00.03\x1e  \x1faСанкт-Петербург\x1fc1992\x1e  \x1fa24 c.\x1fbил\x1e 7\x1faВсеобщая история\x1f2nsnr\x1e41\x1fqapplication/pdf\x1fuhttp://dlib.rsl.ru/rsl01000000000/rsl01000000000/rsl01000000001/rsl01000000001.pdf\x1e  \x1faautoref\x1e  \x1fbautoreg\x1fbautoreh\x1e\x1d\
                                00963nam a2200229 i 4500001001000000003000800010003000800018005001700026008004100043035002300084040002600107041000800133072001900141100005800160245028000218260004000498300001600538650004200554856010400596979001200700979002100712\x1e000000002\x1eRuMoRGB\x1eEnMoRGB\x1e20080528120000.0\x1e080528s1992    ru a|||  a    |00 u rus d\x1e  \x1fa(RuMoEDL)-92k71098\x1e  \x1faRuMoRGB\x1fbrus\x1fcRuMoRGB\x1e0 \x1farus\x1e 7\x1fa07.00.03\x1f2nsnr\x1e1 \x1fa'Абд Ал-'Азиз Джа'фар Бин 'Акид\x1e00\x1faЭтносоциальная структура и институты социальной защиты в Хадрамауте (19 - первая половина 20 вв.) :\x1fbавтореферат дис. ... кандидата исторических наук : 07.00.03\x1e  \x1faСанкт-Петербург\x1fc1992\x1e  \x1fa24 c.\x1fbил\x1e 7\x1faВсеобщая история\x1f2nsnr\x1e41\x1fqapplication/pdf\x1fuhttp://dlib.rsl.ru/rsl01000000000/rsl01000000000/rsl01000000002/rsl01000000002.pdf\x1e  \x1faautoref\x1e  \x1fbautoreg\x1fbautoreh\x1e\x1d";
    const FIELDS_COUNT: usize = 17;
    mod read {
        use test;
        use super::RECS;
        use super::FIELDS_COUNT;
        use super::super::*;

        #[test]
        fn records() {
            let mut recs = RECS.as_bytes();
            let rec1 = recs.read_record().unwrap().unwrap();
            assert_eq!(rec1.data, &RECS.as_bytes()[0..rec1.data.len()]);
            assert_eq!(rec1.data.len(), 963);
            let rec2 = recs.read_record().unwrap().unwrap();
            assert_eq!(rec2.data.len(), 963);
            assert_eq!(rec2.data, &RECS.as_bytes()[rec1.data.len()..]);
            assert_eq!(None, recs.read_record().unwrap());
        }

        #[test]
        fn from_vec() {
            use std::borrow::ToOwned;

            let vec = (& RECS.as_bytes()[0..963]).to_owned();
            let rec = Record::from_vec(vec).unwrap();
            assert_eq!(rec.data, &RECS.as_bytes()[0..rec.data.len()]);
            assert_eq!(rec.data.len(), 963);
        }

        #[test]
        fn fields_iterator() {
            let rec = RECS.as_bytes().read_record().unwrap().unwrap();
            let mut fields = rec.fields();
            let hint = fields.size_hint();
            assert_eq!(hint.0, *hint.1.as_ref().unwrap());
            assert_eq!(hint.0, FIELDS_COUNT);
            let mut count = 0;
            while let Some(field) = fields.next() {
                count += 1;
                let hint = fields.size_hint();
                assert_eq!(hint.0, *hint.1.as_ref().unwrap());
                assert_eq!(hint.0, FIELDS_COUNT - count);
                match count {
                    01 => assert_eq!(Some(field.get_tag()), Some("001")),
                    02 => assert_eq!(Some(field.get_tag()), Some("003")),
                    03 => assert_eq!(Some(field.get_tag()), Some("003")),
                    04 => assert_eq!(Some(field.get_tag()), Some("005")),
                    05 => assert_eq!(Some(field.get_tag()), Some("008")),
                    06 => assert_eq!(Some(field.get_tag()), Some("035")),
                    07 => assert_eq!(Some(field.get_tag()), Some("040")),
                    08 => assert_eq!(Some(field.get_tag()), Some("041")),
                    09 => assert_eq!(Some(field.get_tag()), Some("072")),
                    10 => assert_eq!(Some(field.get_tag()), Some("100")),
                    11 => assert_eq!(Some(field.get_tag()), Some("245")),
                    12 => assert_eq!(Some(field.get_tag()), Some("260")),
                    13 => assert_eq!(Some(field.get_tag()), Some("300")),
                    14 => assert_eq!(Some(field.get_tag()), Some("650")),
                    15 => assert_eq!(Some(field.get_tag()), Some("856")),
                    16 => assert_eq!(Some(field.get_tag()), Some("979")),
                    17 => assert_eq!(Some(field.get_tag()), Some("979")),
                    _ => assert!(false),
                }
            }
        }

        #[test]
        fn control_field() {
            let rec = RECS.as_bytes().read_record().unwrap().unwrap();
            assert_eq!(rec.get_field("001").len(), 1);
            assert!(rec.get_field("001")[0].is_control_field());
            assert_eq!(rec.get_field("001")[0].data, b"000000001\x1e");
            assert_eq!(Some(rec.get_field("001")[0].get_tag()), Some(&b"001"[..]));
            assert_eq!(rec.get_field("001")[0].get_data(), Some(&b"000000001"[..]));
            assert_eq!(Some(rec.get_field("001")[0].get_tag()), Some("001"));
            assert_eq!(rec.get_field("001")[0].get_data(), Some("000000001"));
        }

        #[test]
        fn control_fields() {
            let rec = RECS.as_bytes().read_record().unwrap().unwrap();
            assert_eq!(rec.get_field("003").len(), 2);
            assert!(rec.get_field("003")[0].is_control_field());
            assert_eq!(rec.get_field("003")[0].data, b"RuMoRGB\x1e");
            assert_eq!(Some(rec.get_field("003")[0].get_tag()), Some(&b"003"[..]));
            assert_eq!(rec.get_field("003")[0].get_data(), Some(&b"RuMoRGB"[..]));
            assert_eq!(Some(rec.get_field("003")[0].get_tag()), Some("003"));
            assert_eq!(rec.get_field("003")[0].get_data(), Some("RuMoRGB"));
            assert!(rec.get_field("003")[1].is_control_field());
            assert_eq!(rec.get_field("003")[1].data, b"EnMoRGB\x1e");
            assert_eq!(Some(rec.get_field("003")[1].get_tag()), Some(&b"003"[..]));
            assert_eq!(rec.get_field("003")[1].get_data(), Some(&b"EnMoRGB"[..]));
            assert_eq!(Some(rec.get_field("003")[1].get_tag()), Some("003"));
            assert_eq!(rec.get_field("003")[1].get_data(), Some("EnMoRGB"));
        }

        #[test]
        fn data_field() {
            let rec = RECS.as_bytes().read_record().unwrap().unwrap();
            assert_eq!(rec.get_field("856").len(), 1);
            assert!(rec.get_field("856")[0].is_data_field());
            assert_eq!(Some(rec.get_field("856")[0].get_tag()), Some("856"));
            assert_eq!(rec.get_field("856")[0].data, &b"41\x1fqapplication/pdf\x1fuhttp://dlib.rsl.ru/rsl01000000000/rsl01000000000/rsl01000000001/rsl01000000001.pdf\x1e"[..]);
            assert_eq!(rec.get_field("856")[0].get_indicator(), Some(['4', '1']));
        }

        #[test]
        fn data_fields() {
            let rec = RECS.as_bytes().read_record().unwrap().unwrap();
            assert_eq!(rec.get_field("979").len(), 2);
            assert!(rec.get_field("979")[0].is_data_field());
            assert_eq!(Some(rec.get_field("979")[0].get_tag()), Some("979"));
            assert_eq!(rec.get_field("979")[0].data, b"  \x1faautoref\x1e");
            assert_eq!(rec.get_field("979")[0].get_indicator(), Some([' ', ' ']));
            assert!(rec.get_field("979")[1].is_data_field());
            assert_eq!(Some(rec.get_field("979")[1].get_tag()), Some("979"));
            assert_eq!(rec.get_field("979")[1].data, b"  \x1fbautoreg\x1fbautoreh\x1e");
            assert_eq!(rec.get_field("979")[1].get_indicator(), Some([' ', ' ']));
        }

        #[test]
        fn subfields_iterator() {
            let rec = RECS.as_bytes().read_record().unwrap().unwrap();
            let ref field = rec.get_field("979")[0];
            let mut subfields = field.subfields();
            let hint = subfields.size_hint();
            assert_eq!(hint.0, *hint.1.as_ref().unwrap());
            assert_eq!(hint.0, 1);
            let mut count = 0;
            while let Some(subfield) = subfields.next() {
                count += 1;
                let hint = subfields.size_hint();
                assert_eq!(hint.0, *hint.1.as_ref().unwrap());
                assert_eq!(hint.0, 0);
                match count {
                    01 => {
                        assert_eq!(subfield.get_identifier::<char>(), 'a');
                        assert_eq!(subfield.get_data(), Some("autoref"));
                    },
                    _ => assert!(false),
                }
            }
            let ref field = rec.get_field("979")[1];
            let mut subfields = field.subfields();
            let hint = subfields.size_hint();
            assert_eq!(hint.0, *hint.1.as_ref().unwrap());
            assert_eq!(hint.0, 2);
            let mut count = 0;
            while let Some(subfield) = subfields.next() {
                count += 1;
                let hint = subfields.size_hint();
                assert_eq!(hint.0, *hint.1.as_ref().unwrap());
                assert_eq!(hint.0, 2 - count);
                match count {
                    01 => {
                        assert_eq!(subfield.get_identifier::<char>(), 'b');
                        assert_eq!(subfield.get_data(), Some("autoreg"));
                    },
                    02 => {
                        assert_eq!(subfield.get_identifier::<char>(), 'b');
                        assert_eq!(subfield.get_data(), Some("autoreh"));
                    },
                    _ => assert!(false),
                }
            }
        }

        #[test]
        fn subfield() {
            let rec = RECS.as_bytes().read_record().unwrap().unwrap();
            let ref field = rec.get_field("979")[0];
            assert_eq!(field.get_subfield('a').len(), 1);
            assert_eq!(field.get_subfield('a')[0].data, b"\x1faautoref");
            assert_eq!(Some(field.get_subfield('a')[0].get_tag()), Some("979"));
            assert_eq!(field.get_subfield('a')[0].get_identifier::<char>(), 'a');
            assert_eq!(field.get_subfield('a')[0].get_data(), Some("autoref"));
        }

        #[test]
        fn subfields() {
            let rec = RECS.as_bytes().read_record().unwrap().unwrap();
            let ref field = rec.get_field("979")[1];
            assert_eq!(field.get_subfield('b').len(), 2);
            assert_eq!(field.get_subfield('b')[0].data, b"\x1fbautoreg");
            assert_eq!(Some(field.get_subfield('b')[0].get_tag()), Some("979"));
            assert_eq!(field.get_subfield('b')[0].get_identifier::<char>(), 'b');
            assert_eq!(field.get_subfield('b')[0].get_data(), Some("autoreg"));
            assert_eq!(field.get_subfield('b')[1].data, b"\x1fbautoreh");
            assert_eq!(Some(field.get_subfield('b')[1].get_tag()), Some("979"));
            assert_eq!(field.get_subfield('b')[1].get_identifier::<char>(), 'b');
            assert_eq!(field.get_subfield('b')[1].get_data(), Some("autoreh"));
        }

        #[bench]
        fn read_record(b: &mut test::Bencher) {
            b.iter(|| {
                if let Ok(rec) = RECS.as_bytes().read_record() {
                    if let Some(rec) = rec {
                        test::black_box(rec);
                    } else {
                        assert!(false);
                    }
                }
            });
            b.bytes += RECS.as_bytes().len() as u64;
        }

        #[bench]
        fn read_record_get_field(b: &mut test::Bencher) {
            b.iter(|| {
                if let Ok(rec) = RECS.as_bytes().read_record() {
                    if let Some(rec) = rec {
                        assert_eq!(rec.get_field("979").len(), 2);
                    } else {
                        assert!(false);
                    }
                }
            });
            b.bytes += RECS.as_bytes().len() as u64;
        }

        #[bench]
        fn read_record_iter_fields(b: &mut test::Bencher) {
            b.iter(|| {
                if let Ok(rec) = RECS.as_bytes().read_record() {
                    if let Some(rec) = rec {
                        for field in rec.fields() {
                            test::black_box(field);
                        }
                    } else {
                        assert!(false);
                    }
                }
            });
            b.bytes += RECS.as_bytes().len() as u64;
        }

        #[bench]
        fn read_record_iter_subfields(b: &mut test::Bencher) {
            b.iter(|| {
                if let Ok(rec) = RECS.as_bytes().read_record() {
                    if let Some(rec) = rec {
                        for field in rec.fields() {
                            for subfield in field.subfields() {
                                test::black_box(subfield);
                            }
                        }
                    } else {
                        assert!(false);
                    }
                }
            });
            b.bytes += RECS.as_bytes().len() as u64;
        }

        #[bench]
        fn build_record_from_record(b: &mut test::Bencher) {
            let record = RECS.as_bytes().read_record().unwrap().unwrap();
            b.iter(|| {
                test::black_box(RecordBuilder::from_record(&record));
            })
        }
    }
}
