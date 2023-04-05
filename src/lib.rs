//! # Library to work with the MARC 21 Format for Bibliographic Data
//!
//! ## Examples
//!
//! ### Reading
//!
//! ```rust
//! # use marc::*;
//! # use std::{io, fs};
//! # fn main() -> marc::Result<()> {
//! let input = fs::File::open("test/fixtures/3records.mrc")?;
//! let mut count = 0;
//!
//! for (i, record) in Records::new(input).enumerate() {
//!     let record = dbg!(record?);
//!     match i {
//!         0 => assert_eq!(record.field(b"001")[0].get_data::<str>(), "000000002"),
//!         1 => assert_eq!(record.field(b"001")[0].get_data::<str>(), "000000005"),
//!         2 => assert_eq!(record.field(b"001")[0].get_data::<str>(), "000000009"),
//!         _ => panic!(),
//!     }
//!     count += 1;
//! }
//!
//! assert_eq!(count, 3);
//! # marc::Result::Ok(())
//! # }
//! ```
//!
//! ### Creating
//!
//! ```rust
//! # use marc::*;
//! # use std::{io, fs};
//! # fn main() -> marc::Result<()> {
//! let mut builder = RecordBuilder::new();
//! let record = builder
//!     .add_fields(fields!(
//!         control fields: [
//!             b"001" => "000000002",
//!             b"003" => "RuMoRGB",
//!         ];
//!         data fields: [
//!             b"979", b"  ", [
//!                 b'a' => "autoref",
//!                 b'a' => "dlopen",
//!             ],
//!         ];
//!     ))?
//!     .get_record()?;
//! assert_eq!(record.as_ref(), b"00100nam  2200061 i 4500001001000000003000800010\
//!     979002000018\x1E000000002\x1ERuMoRGB\x1E  \x1Faautoref\x1Fadlopen\x1E\x1D");
//! # marc::Result::Ok(())
//! # }
//! ```
//!
//! ### Updating
//!
//! ```rust
//! # use marc::*;
//! # use std::{io, fs};
//! # fn main() -> marc::Result<()> {
//! let input = fs::File::open("test/fixtures/3records.mrc")?;
//! let orig_record = Records::new(input).next().expect("should be here")?;
//! let mut builder = RecordBuilder::from_record(&orig_record);
//! let record = builder
//!     // we'll replace `001`
//!     .filter_fields(|f| f.get_tag() != "001")
//!     .add_field((b"001", "foo"))?
//!     // we'll remove `979a` with value `dlopen` (note that an empty `979` will remain)
//!     .filter_subfields(|_, sf| sf.get_tag() != "979" ||
//!         sf.get_identifier() != 'a' ||
//!         sf.get_data::<str>() != "dlopen")
//!     .get_record()?;
//!
//! assert_eq!(record.as_ref(), "01339nam a2200301 i 45000010004000000030008000040050017000120080\
//!     041000290170023000700350025000930400026001180410008001440720019001520840027001710840029001\
//!     980840029002271000076002562450352003322600025006843000011007096500092007207870038008128520\
//!     03400850852003400884856010400918979001201022979000301034\x1efoo\x1eRuMoRGB\x1e201507161647\
//!     15.0\x1e911009s1990    ru ||||  a    |00 u rus d\x1e  \x1fa91-8563А\x1fbRuMoRKP\x1e  \x1fa\
//!     (RuMoRGB)DIS-0000114\x1e  \x1faRuMoRGB\x1fbrus\x1fcRuMoRGB\x1e0 \x1farus\x1e 7\x1fa07.00.0\
//!     3\x1f2nsnr\x1e  \x1faЭ38-36-021.4,0\x1f2rubbk\x1e  \x1faТ3(6Ег)63-4,02\x1f2rubbk\x1e  \x1f\
//!     aТ3(5Ср)63-4,02\x1f2rubbk\x1e1 \x1faАбдувахитов, Абдужабар Абдусаттарович\x1e00\x1fa\"Брат\
//!     ья-мусульмане\" на общественно-политической арене Египта и Сирии в 1928-1963 гг. :\x1fbавт\
//!     ореферат дис. ... кандидата исторических наук : 07.00.03\x1fcАбдувахитов Абдужабар Абдусат\
//!     тарович ; Ташк. гос. ун-т\x1e  \x1faТашкент\x1fc1990\x1e  \x1fa17 с.\x1e 7\x1faВсеобщая ис\
//!     тория (соответствующего периода)\x1f2nsnr\x1e18\x1fw008120708\x1fiДиссертация\x1e4 \x1faРГ\
//!     Б\x1fbFB\x1fj9 91-4/2388-x\x1fx71\x1e4 \x1faРГБ\x1fbFB\x1fj9 91-4/2389-8\x1fx70\x1e41\x1fq\
//!     application/pdf\x1fuhttp://dlib.rsl.ru/rsl01000000000/rsl01000000000/rsl01000000002/rsl010\
//!     00000002.pdf\x1e  \x1faautoref\x1e  \x1e\x1d".as_bytes());
//!
//! # marc::Result::Ok(())
//! # }
//! ```

#![warn(missing_debug_implementations, rust_2018_idioms, future_incompatible)]
#![cfg_attr(feature = "nightly", feature(test))]
#![recursion_limit = "1024"]

#[cfg(feature = "nightly")]
extern crate test;

use std::{
    borrow::{Borrow, Cow},
    fmt, io, slice,
};

mod directory;
pub mod errors;
mod field;
mod identifier;
mod indicator;
mod misc;
mod tag;

pub use errors::*;

#[doc(inline)]
pub use field::fields::Fields;
#[doc(inline)]
pub use field::subfield::subfields::Subfields;
#[doc(inline)]
pub use field::subfield::Subfield;
#[doc(inline)]
pub use field::Field;
#[doc(inline)]
pub use field::FieldRepr;
#[doc(inline)]
pub use field::FromFieldData;
#[doc(inline)]
pub use identifier::Identifier;
#[doc(inline)]
pub use indicator::Indicator;
#[doc(inline)]
pub use tag::Tag;

use directory::Directory;

const MAX_FIELD_LEN: usize = 9_999;
const MAX_RECORD_LEN: usize = 99_999;
const RECORD_TERMINATOR: u8 = 0x1D;
const FIELD_TERMINATOR: u8 = 0x1E;
const SUBFIELD_DELIMITER: u8 = 0x1F;

macro_rules! get {
    ($name:ident, $sname:ident, $num:expr) => {
        pub fn $sname(&self) -> $name {
            self.data[$num].into()
        }
    };
}

/// Parsed MARC Record.
///
/// It could be borrowed if it was parsed from a buffer or it could be owned if it was read from an
/// `io::Read` implementor.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Record<'a> {
    data: Cow<'a, [u8]>,
    data_offset: usize,
    directory: Directory,
}

impl<'a> Record<'a> {
    /// Will try to parse record from a buffer.
    ///
    /// Will borrow an `input` for the lifetime of a produced record.
    pub fn parse(input: &[u8]) -> Result<Record<'_>> {
        let len = misc::read_dec_5(input)?;
        if input.len() < len {
            return Err(Error::UnexpectedEof);
        }

        let data = &input[..len];
        if data[len - 1] != RECORD_TERMINATOR {
            return Err(Error::NoRecordTerminator);
        }

        let data_offset = misc::read_dec_5(&data[12..17])?;
        let directory = Directory::parse(&data[24..data_offset])?;

        Ok(Record {
            data: Cow::Borrowed(data),
            data_offset,
            directory,
        })
    }

    /// Will crate owned record from vector of bytes.
    ///
    /// # Panic
    /// Will check that input length equals the record length.
    pub fn from_vec<I>(input: I) -> Result<Record<'static>>
    where
        I: Into<Vec<u8>>,
    {
        let input = input.into();

        let (data_offset, directory) = {
            let Record {
                data_offset,
                directory,
                data,
            } = Record::parse(&*input)?;
            assert_eq!(input.len(), data.as_ref().len());
            (data_offset, directory)
        };

        Ok(Record {
            data: Cow::Owned(input),
            data_offset,
            directory,
        })
    }

    /// Will try to read a `Record` from an `io::Read` implementor.
    ///
    /// Will return `None` if reader is empty.
    pub fn read<T: io::Read>(input: &mut T) -> Result<Option<Record<'static>>> {
        let mut data = vec![0; 5];

        if let 0 = input.read(&mut data[..1])? {
            return Ok(None);
        }

        input.read_exact(&mut data[1..])?;

        let len = misc::read_dec_5(&*data)?;

        if len < 5 {
            return Err(Error::RecordTooShort(len));
        }

        data.resize(len, 0);
        input.read_exact(&mut data[5..len])?;

        let data_offset = misc::read_dec_5(&data[12..17])?;
        let directory = Directory::parse(&data[24..data_offset])?;

        Ok(Some(Record {
            data: Cow::Owned(data),
            data_offset,
            directory,
        }))
    }

    /// Will return fields with tag == `Tag`
    pub fn field<T: Into<tag::Tag>>(&self, tag: T) -> Vec<Field<'_>> {
        let tag = tag.into();
        let mut output = Vec::with_capacity(4);
        for entry in self.directory.entries.iter() {
            if entry.0 == tag {
                let offset = self.data_offset + entry.2;
                output.push(Field::new(tag, &self.data[offset..offset + entry.1 - 1]));
            }
        }
        output
    }

    /// Will return iterator over fields of a record
    pub fn fields(&self) -> Fields<'_> {
        Fields::new(self)
    }

    get!(RecordStatus, record_status, 5);
    get!(TypeOfRecord, type_of_record, 6);
    get!(BibliographicLevel, bibliographic_level, 7);
    get!(TypeOfControl, type_of_control, 8);
    get!(CharacterCodingScheme, character_coding_scheme, 9);
    get!(EncodingLevel, encoding_level, 17);
    get!(DescriptiveCatalogingForm, descriptive_cataloging_form, 18);
    get!(
        MultipartResourceRecordLevel,
        multipart_resource_record_level,
        19
    );
}

impl AsRef<[u8]> for Record<'_> {
    fn as_ref(&self) -> &[u8] {
        self.data.borrow()
    }
}

impl<'a> fmt::Display for Record<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let leader = &self.as_ref()[0..24];
        writeln!(f, "=LDR  {}", String::from_utf8_lossy(leader))?;
        for field in self.fields() {
            writeln!(f, "{}", field)?;
        }
        Ok(())
    }
}

/// Write Record Extension on io::Write
pub trait WriteRecordExt: io::Write {
    /// write a record to a io::Write implementor
    ///
    /// returns the length of the written record
    fn write_record(&mut self, record: Record<'_>) -> io::Result<()>;
}

impl<T> WriteRecordExt for T
where
    T: io::Write,
{
    fn write_record(&mut self, record: Record<'_>) -> io::Result<()> {
        self.write_all(record.as_ref())
    }
}

/// Reads records from an `io::Read` implementor.
#[derive(Debug, Clone)]
pub struct Records<T>(T, bool);

impl<T: io::Read> Records<T> {
    pub fn new(input: T) -> Records<T> {
        Records(input, false)
    }

    /// Unwraps `io::Read` implementor.
    pub fn unwrap(self) -> T {
        self.0
    }
}

impl<T: io::Read> Iterator for Records<T> {
    type Item = Result<Record<'static>>;

    fn next(&mut self) -> Option<Result<Record<'static>>> {
        if self.1 {
            None
        } else {
            let result = Record::read(&mut self.0);
            match result {
                Ok(Some(record)) => Some(Ok(record)),
                Ok(None) => {
                    self.1 = true;
                    None
                }
                Err(err) => {
                    self.1 = true;
                    Some(Err(err))
                }
            }
        }
    }
}

macro_rules! getset {
    ($name:ident, $geti:ident, $seti:ident, $num:expr) => {
        pub fn $geti(&self) -> $name {
            self.leader[$num].into()
        }
        pub fn $seti(&mut self, x: $name) -> &mut Self {
            self.leader[$num] = x.into();
            self
        }
    };
}

/// Record builder.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordBuilder {
    leader: [u8; 24],
    fields: Vec<FieldRepr>,
}

impl RecordBuilder {
    /// Creates default record builer
    pub fn new() -> RecordBuilder {
        RecordBuilder {
            leader: *b"00000nam  2200000 i 4500",
            fields: vec![],
        }
    }

    /// Creates record builder from existing record
    pub fn from_record(record: &Record<'_>) -> RecordBuilder {
        let mut leader = [0; 24];
        leader.copy_from_slice(&record.as_ref()[0..24]);
        let fields = record.fields().map(FieldRepr::from).collect();
        RecordBuilder { leader, fields }
    }

    /// Iterator over fields of this builder.
    pub fn iter_fields(&self) -> slice::Iter<'_, FieldRepr> {
        self.fields.iter()
    }

    /// A way to add field to this builder.
    ///
    /// ### Errors
    ///
    /// Will return error if field is larger than 9.999 bytes.
    pub fn add_field<T: Into<FieldRepr>>(&mut self, f: T) -> Result<&mut Self> {
        let repr = f.into();
        if repr.get_data().len() + 1 > MAX_FIELD_LEN {
            return Err(Error::FieldTooLarge(repr.get_tag()));
        }
        self.fields.push(repr);
        self.fields.sort_by_key(|f| f.get_tag());
        Ok(self)
    }

    /// A way to add multiple fileds to this builder.
    ///
    /// ### Errors
    ///
    /// Will return error if any of fields is larger than 9.999 bytes.
    pub fn add_fields<T: Into<FieldRepr>>(&mut self, fs: Vec<T>) -> Result<&mut Self> {
        for f in fs {
            self.add_field(f)?;
        }
        Ok(self)
    }

    /// Will filter fields of this builder by `fun` predicate.
    pub fn filter_fields<F>(&mut self, mut fun: F) -> &mut RecordBuilder
    where
        F: FnMut(&Field<'_>) -> bool,
    {
        let fields = self
            .fields
            .clone()
            .into_iter()
            .filter(|ref f| {
                let f = Field::from_repr(f);
                fun(&f)
            })
            .collect();
        self.fields = fields;
        self
    }

    /// Will filter subfields of this builder by `fun` predicate.
    pub fn filter_subfields<F>(&mut self, mut fun: F) -> &mut Self
    where
        F: FnMut(&Field<'_>, &Subfield<'_>) -> bool,
    {
        let fields = self
            .fields
            .clone()
            .into_iter()
            .map(|f| {
                let fld = Field::from_repr(&f);
                f.filter_subfields(|sf| fun(&fld, &sf))
            })
            .collect();
        self.fields = fields;
        self
    }

    /// Returns record.
    ///
    /// ### Errors
    ///
    /// Will return error if record length is greater than 99.999 bytes.
    pub fn get_record(&self) -> Result<Record<'static>> {
        let mut data: Vec<_> = self.leader.to_vec();

        // leader + directory terminator + record terminator
        let mut size = 24 + 1 + 1;
        for f in self.fields.iter() {
            // directory entry + field data length + field terminator
            size += 12 + f.get_data().len() + 1;
        }
        if size > MAX_RECORD_LEN {
            return Err(Error::RecordTooLarge(size));
        }

        // writing record length
        data[0..5].copy_from_slice(format!("{:05}", size).as_bytes());

        // writing directory
        let mut offset = 0;
        for f in self.fields.iter() {
            data.extend_from_slice(f.get_tag().as_ref());
            // field data length + field terminator
            data.extend_from_slice(format!("{:04}", f.get_data().len() + 1).as_bytes());
            data.extend_from_slice(format!("{:05}", offset).as_bytes());
            offset += f.get_data().len() + 1;
        }
        data.push(FIELD_TERMINATOR);

        // writing base address of data
        let len = data.len();
        data[12..17].copy_from_slice(format!("{:05}", len).as_bytes());

        // writing fields
        for f in self.fields.iter() {
            data.extend_from_slice(f.get_data());
            data.push(FIELD_TERMINATOR);
        }

        data.push(RECORD_TERMINATOR);

        let (data_offset, directory) = match Record::parse(&*data) {
            Ok(Record {
                data: _,
                data_offset,
                directory,
            }) => (data_offset, directory),
            Err(err) => return Err(err),
        };

        Ok(Record {
            data: Cow::Owned(data),
            data_offset,
            directory,
        })
    }

    getset!(RecordStatus, get_record_status, set_record_status, 5);
    getset!(TypeOfRecord, get_type_of_record, set_type_of_record, 6);
    getset!(
        BibliographicLevel,
        get_bibliographic_level,
        set_bibliographic_level,
        7
    );
    getset!(TypeOfControl, get_type_of_control, set_type_of_control, 8);
    getset!(
        CharacterCodingScheme,
        get_character_coding_scheme,
        set_character_coding_scheme,
        9
    );
    getset!(EncodingLevel, get_encoding_level, set_encoding_level, 17);
    getset!(
        DescriptiveCatalogingForm,
        get_descriptive_cataloging_form,
        set_descriptive_cataloging_form,
        18
    );
    getset!(
        MultipartResourceRecordLevel,
        get_multipart_resource_record_level,
        set_multipart_resource_record_level,
        19
    );
}

impl Default for RecordBuilder {
    fn default() -> Self {
        Self::new()
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

        impl From<u8> for $name {
            fn from(x: u8) -> $name {
                match x {
                    $($val => $name::$kind),+,
                    b => $name::Unknown(b),
                }
            }
        }

        impl From<$name> for u8 {
            fn from(x: $name) -> u8 {
                match x {
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

#[macro_export]
/// Intended to use with `RecordBuilder::add_fields`.
///
/// ```rust
/// # use marc::{fields, RecordBuilder};
/// # let mut builder = RecordBuilder::new();
/// builder.add_fields(fields!(
///     control fields: [b"001" => "foo"];
///     data fields: [
///         b"856", b"41", [
///             b'q' => "bar",
///             b'u' => "baz",
///         ],
///     ];
/// ));
/// ```
macro_rules! fields {
    (
        control fields: [$($ctag:expr => $cdata:expr),*];
    ) => (
        fields!( control fields: [ $($ctag => $cdata,)* ];
                 data fields: [ ]; )
    );
    (
        control fields: [$($ctag:expr => $cdata:expr,)*];
    ) => (
        fields!( control fields: [ $($ctag => $cdata,)* ];
                 data fields: [ ]; )
    );

    (
        data fields: [ $( $dtag:expr, $dind:expr, [$($sfident:expr => $sfdata:expr),*] ),* ];
    ) => (
        fields!( control fields: [ ];
                 data fields: [ $($dtag, $dind, [ $($sfident => $sfdata,)* ],)* ]; )
    );
    (
        data fields: [ $( $dtag:expr, $dind:expr, [$($sfident:expr => $sfdata:expr,)*] ),* ];
    ) => (
        fields!( control fields: [ ];
                 data fields: [ $($dtag, $dind, [ $($sfident => $sfdata,)* ],)* ]; )
    );
    (
        data fields: [ $( $dtag:expr, $dind:expr, [$($sfident:expr => $sfdata:expr),*],)* ];
    ) => (
        fields!( control fields: [ ];
                 data fields: [ $($dtag, $dind, [ $($sfident => $sfdata,)* ],)* ]; )
    );
    (
        data fields: [ $( $dtag:expr, $dind:expr, [$($sfident:expr => $sfdata:expr,)*],)* ];
    ) => (
        fields!( control fields: [ ];
                 data fields: [ $($dtag, $dind, [ $($sfident => $sfdata,)* ],)* ]; )
    );

    (
        data fields: [ $( $dtag:expr, $dind:expr, [$($sfident:expr => $sfdata:expr),*] ),* ];
        control fields: [$($ctag:expr => $cdata:expr),*];
    ) => (
        fields!( control fields: [ $($ctag => $cdata,)* ];
                 data fields: [ $($dtag, $dind, [ $($sfident => $sfdata,)* ],)* ]; )
    );
    (
        data fields: [ $( $dtag:expr, $dind:expr, [$($sfident:expr => $sfdata:expr),*] ),* ];
        control fields: [$($ctag:expr => $cdata:expr,)*];
    ) => (
        fields!( control fields: [ $($ctag => $cdata,)* ];
                 data fields: [ $($dtag, $dind, [ $($sfident => $sfdata,)* ],)* ]; )
    );
    (
        data fields: [ $( $dtag:expr, $dind:expr, [$($sfident:expr => $sfdata:expr,)*] ),* ];
        control fields: [$($ctag:expr => $cdata:expr),*];
    ) => (
        fields!( control fields: [ $($ctag => $cdata,)* ];
                 data fields: [ $($dtag, $dind, [ $($sfident => $sfdata,)* ],)* ]; )
    );
    (
        data fields: [ $( $dtag:expr, $dind:expr, [$($sfident:expr => $sfdata:expr,)*] ),* ];
        control fields: [$($ctag:expr => $cdata:expr,)*];
    ) => (
        fields!( control fields: [ $($ctag => $cdata,)* ];
                 data fields: [ $($dtag, $dind, [ $($sfident => $sfdata,)* ],)* ]; )
    );
    (
        data fields: [ $( $dtag:expr, $dind:expr, [$($sfident:expr => $sfdata:expr),*],)* ];
        control fields: [$($ctag:expr => $cdata:expr),*];
    ) => (
        fields!( control fields: [ $($ctag => $cdata,)* ];
                 data fields: [ $($dtag, $dind, [ $($sfident => $sfdata,)* ],)* ]; )
    );
    (
        data fields: [ $( $dtag:expr, $dind:expr, [$($sfident:expr => $sfdata:expr),*],)* ];
        control fields: [$($ctag:expr => $cdata:expr,)*];
    ) => (
        fields!( control fields: [ $($ctag => $cdata,)* ];
                 data fields: [ $($dtag, $dind, [ $($sfident => $sfdata,)* ],)* ]; )
    );
    (
        data fields: [ $( $dtag:expr, $dind:expr, [$($sfident:expr => $sfdata:expr,)*],)* ];
        control fields: [$($ctag:expr => $cdata:expr),*];
    ) => (
        fields!( control fields: [ $($ctag => $cdata,)* ];
                 data fields: [ $($dtag, $dind, [ $($sfident => $sfdata,)* ],)* ]; )
    );
    (
        data fields: [ $( $dtag:expr, $dind:expr, [$($sfident:expr => $sfdata:expr,)*], )* ];
        control fields: [$($ctag:expr => $cdata:expr,)*];
    ) => (
        fields!( control fields: [ $($ctag => $cdata,)* ];
                 data fields: [ $($dtag, $dind, [ $($sfident => $sfdata,)* ],)* ]; )
    );


    (
        control fields: [$($ctag:expr => $cdata:expr),*];
        data fields: [ $( $dtag:expr, $dind:expr, [$($sfident:expr => $sfdata:expr),*] ),* ];
    ) => (
        fields!( control fields: [ $($ctag => $cdata,)* ];
                 data fields: [ $($dtag, $dind, [ $($sfident => $sfdata,)* ],)* ]; )
    );
    (
        control fields: [$($ctag:expr => $cdata:expr,)*];
        data fields: [ $( $dtag:expr, $dind:expr, [$($sfident:expr => $sfdata:expr),*] ),* ];
    ) => (
        fields!( control fields: [ $($ctag => $cdata,)* ];
                 data fields: [ $($dtag, $dind, [ $($sfident => $sfdata,)* ],)* ]; )
    );
    (
        control fields: [$($ctag:expr => $cdata:expr),*];
        data fields: [ $( $dtag:expr, $dind:expr, [$($sfident:expr => $sfdata:expr,)*] ),* ];
    ) => (
        fields!( control fields: [ $($ctag => $cdata,)* ];
                 data fields: [ $($dtag, $dind, [ $($sfident => $sfdata,)* ],)* ]; )
    );
    (
        control fields: [$($ctag:expr => $cdata:expr,)*];
        data fields: [ $( $dtag:expr, $dind:expr, [$($sfident:expr => $sfdata:expr,)*] ),* ];
    ) => (
        fields!( control fields: [ $($ctag => $cdata,)* ];
                 data fields: [ $($dtag, $dind, [ $($sfident => $sfdata,)* ],)* ]; )
    );
    (
        control fields: [$($ctag:expr => $cdata:expr),*];
        data fields: [ $( $dtag:expr, $dind:expr, [$($sfident:expr => $sfdata:expr),*],)* ];
    ) => (
        fields!( control fields: [ $($ctag => $cdata,)* ];
                 data fields: [ $($dtag, $dind, [ $($sfident => $sfdata,)* ],)* ]; )
    );
    (
        control fields: [$($ctag:expr => $cdata:expr,)*];
        data fields: [ $( $dtag:expr, $dind:expr, [$($sfident:expr => $sfdata:expr),*],)* ];
    ) => (
        fields!( control fields: [ $($ctag => $cdata,)* ];
                 data fields: [ $($dtag, $dind, [ $($sfident => $sfdata,)* ],)* ]; )
    );
    (
        control fields: [$($ctag:expr => $cdata:expr),*];
        data fields: [ $( $dtag:expr, $dind:expr, [$($sfident:expr => $sfdata:expr,)*],)* ];
    ) => (
        fields!( control fields: [ $($ctag => $cdata,)* ];
                 data fields: [ $($dtag, $dind, [ $($sfident => $sfdata,)* ],)* ]; )
    );
    (
        control fields: [$($ctag:expr => $cdata:expr,)*];
        data fields: [ $( $dtag:expr, $dind:expr, [$($sfident:expr => $sfdata:expr,)*], )* ];
    ) => ({
        let mut out = vec![];
        $(out.push(
            $crate::FieldRepr::from(
                ($crate::Tag::from($ctag), Vec::<u8>::from($cdata))
            )
        );)*
        $({
            let mut sfs = vec![];
            $(sfs.push(($crate::Identifier::from($sfident), Vec::<u8>::from($sfdata)));)*
            out.push(
                $crate::FieldRepr::from(
                    ($crate::Tag::from($dtag), $crate::Indicator::from($dind), sfs)
                )
            )
        })*
        out
    });

}

#[cfg(test)]
mod tests {
    const RECS: &str = "00963nam a2200229 i 4500001001000000003000800010003000800018005001700026008004100043035002300084040002600107041000800133072001900141100005800160245028000218260004000498300001600538650004200554856010400596979001200700979002100712\
                        \x1e000000001\x1eRuMoRGB\x1eEnMoRGB\x1e20080528120000.0\x1e080528s1992    ru a|||  a    |00 u rus d\x1e  \x1fa(RuMoEDL)-92k71098\x1e  \x1faRuMoRGB\x1fbrus\x1fcRuMoRGB\x1e0 \x1farus\x1e 7\x1fa07.00.03\x1f2nsnr\x1e1 \x1fa'Абд Ал-'Азиз Джа'фар Бин 'Акид\x1e00\x1faЭтносоциальная структура и институты социальной защиты в Хадрамауте (19 - первая половина 20 вв.) :\x1fbавтореферат дис. ... кандидата исторических наук : 07.00.03\x1e  \x1faСанкт-Петербург\x1fc1992\x1e  \x1fa24 c.\x1fbил\x1e 7\x1faВсеобщая история\x1f2nsnr\x1e41\x1fqapplication/pdf\x1fuhttp://dlib.rsl.ru/rsl01000000000/rsl01000000000/rsl01000000001/rsl01000000001.pdf\x1e  \x1faautoref\x1e  \x1fbautoreg\x1fbautoreh\x1e\x1d\
                        00963nam a2200229 i 4500001001000000003000800010003000800018005001700026008004100043035002300084040002600107041000800133072001900141100005800160245028000218260004000498300001600538650004200554856010400596979001200700979002100712\
                        \x1e000000002\x1eRuMoRGB\x1eEnMoRGB\x1e20080528120000.0\x1e080528s1992    ru a|||  a    |00 u rus d\x1e  \x1fa(RuMoEDL)-92k71098\x1e  \x1faRuMoRGB\x1fbrus\x1fcRuMoRGB\x1e0 \x1farus\x1e 7\x1fa07.00.03\x1f2nsnr\x1e1 \x1fa'Абд Ал-'Азиз Джа'фар Бин 'Акид\x1e00\x1faЭтносоциальная структура и институты социальной защиты в Хадрамауте (19 - первая половина 20 вв.) :\x1fbавтореферат дис. ... кандидата исторических наук : 07.00.03\x1e  \x1faСанкт-Петербург\x1fc1992\x1e  \x1fa24 c.\x1fbил\x1e 7\x1faВсеобщая история\x1f2nsnr\x1e41\x1fqapplication/pdf\x1fuhttp://dlib.rsl.ru/rsl01000000000/rsl01000000000/rsl01000000002/rsl01000000002.pdf\x1e  \x1faautoref\x1e  \x1fbautoreg\x1fbautoreh\x1e\x1d";
    const REC_SIZE: u64 = 963;
    mod read {
        use super::{super::*, RECS, REC_SIZE};
        use std::io;

        #[test]
        fn should_parse_record() {
            let record = Record::parse(&RECS.as_bytes()[..963]).unwrap();
            assert_eq!(record.record_status(), RecordStatus::New);
            assert_eq!(record.type_of_record(), TypeOfRecord::LanguageMaterial);
            assert_eq!(
                record.bibliographic_level(),
                BibliographicLevel::MonographOrItem
            );
            assert_eq!(record.type_of_control(), TypeOfControl::NoSpecifiedType);
            assert_eq!(
                record.character_coding_scheme(),
                CharacterCodingScheme::UcsUnicode
            );
            assert_eq!(record.encoding_level(), EncodingLevel::FullLevel);
            assert_eq!(
                record.descriptive_cataloging_form(),
                DescriptiveCatalogingForm::IsbdPunctuationIncluded
            );
            assert_eq!(
                record.multipart_resource_record_level(),
                MultipartResourceRecordLevel::NotSpecifiedOrNotApplicable
            );
            assert_eq!(record.as_ref(), &RECS.as_bytes()[0..REC_SIZE as usize]);
        }

        #[test]
        fn should_create_record_from_vec() {
            let record = Record::from_vec((&RECS.as_bytes()[..963]).to_vec()).unwrap();
            assert_eq!(record.record_status(), RecordStatus::New);
            assert_eq!(record.type_of_record(), TypeOfRecord::LanguageMaterial);
            assert_eq!(
                record.bibliographic_level(),
                BibliographicLevel::MonographOrItem
            );
            assert_eq!(record.type_of_control(), TypeOfControl::NoSpecifiedType);
            assert_eq!(
                record.character_coding_scheme(),
                CharacterCodingScheme::UcsUnicode
            );
            assert_eq!(record.encoding_level(), EncodingLevel::FullLevel);
            assert_eq!(
                record.descriptive_cataloging_form(),
                DescriptiveCatalogingForm::IsbdPunctuationIncluded
            );
            assert_eq!(
                record.multipart_resource_record_level(),
                MultipartResourceRecordLevel::NotSpecifiedOrNotApplicable
            );
            assert_eq!(record.as_ref(), &RECS.as_bytes()[0..REC_SIZE as usize]);
        }

        #[test]
        fn should_read_record() {
            let mut data = vec![];
            data.extend_from_slice(RECS.as_bytes());
            let mut input = io::Cursor::new(data);
            let record = Record::read(&mut input).unwrap();
            let record = record.unwrap();
            assert_eq!(record.record_status(), RecordStatus::New);
            assert_eq!(record.type_of_record(), TypeOfRecord::LanguageMaterial);
            assert_eq!(
                record.bibliographic_level(),
                BibliographicLevel::MonographOrItem
            );
            assert_eq!(record.type_of_control(), TypeOfControl::NoSpecifiedType);
            assert_eq!(
                record.character_coding_scheme(),
                CharacterCodingScheme::UcsUnicode
            );
            assert_eq!(record.encoding_level(), EncodingLevel::FullLevel);
            assert_eq!(
                record.descriptive_cataloging_form(),
                DescriptiveCatalogingForm::IsbdPunctuationIncluded
            );
            assert_eq!(
                record.multipart_resource_record_level(),
                MultipartResourceRecordLevel::NotSpecifiedOrNotApplicable
            );
            assert_eq!(record.as_ref(), &RECS.as_bytes()[0..REC_SIZE as usize]);
            let record = Record::read(&mut input).unwrap();
            let record = record.unwrap();
            assert_eq!(record.record_status(), RecordStatus::New);
            assert_eq!(record.type_of_record(), TypeOfRecord::LanguageMaterial);
            assert_eq!(
                record.bibliographic_level(),
                BibliographicLevel::MonographOrItem
            );
            assert_eq!(record.type_of_control(), TypeOfControl::NoSpecifiedType);
            assert_eq!(
                record.character_coding_scheme(),
                CharacterCodingScheme::UcsUnicode
            );
            assert_eq!(record.encoding_level(), EncodingLevel::FullLevel);
            assert_eq!(
                record.descriptive_cataloging_form(),
                DescriptiveCatalogingForm::IsbdPunctuationIncluded
            );
            assert_eq!(
                record.multipart_resource_record_level(),
                MultipartResourceRecordLevel::NotSpecifiedOrNotApplicable
            );
            assert_eq!(record.as_ref(), &RECS.as_bytes()[REC_SIZE as usize..]);
            let record = Record::read(&mut input).unwrap();
            assert!(record.is_none());
        }

        #[test]
        fn should_iterate_records() {
            let mut data = vec![];
            data.extend_from_slice(RECS.as_bytes());
            let input = io::Cursor::new(data);
            let mut records = Records::new(input);

            let record = records.next().unwrap().unwrap();
            assert_eq!(record.record_status(), RecordStatus::New);
            assert_eq!(record.type_of_record(), TypeOfRecord::LanguageMaterial);
            assert_eq!(
                record.bibliographic_level(),
                BibliographicLevel::MonographOrItem
            );
            assert_eq!(record.type_of_control(), TypeOfControl::NoSpecifiedType);
            assert_eq!(
                record.character_coding_scheme(),
                CharacterCodingScheme::UcsUnicode
            );
            assert_eq!(record.encoding_level(), EncodingLevel::FullLevel);
            assert_eq!(
                record.descriptive_cataloging_form(),
                DescriptiveCatalogingForm::IsbdPunctuationIncluded
            );
            assert_eq!(
                record.multipart_resource_record_level(),
                MultipartResourceRecordLevel::NotSpecifiedOrNotApplicable
            );
            assert_eq!(record.as_ref(), &RECS.as_bytes()[0..REC_SIZE as usize]);
            let record = records.next().unwrap().unwrap();
            assert_eq!(record.record_status(), RecordStatus::New);
            assert_eq!(record.type_of_record(), TypeOfRecord::LanguageMaterial);
            assert_eq!(
                record.bibliographic_level(),
                BibliographicLevel::MonographOrItem
            );
            assert_eq!(record.type_of_control(), TypeOfControl::NoSpecifiedType);
            assert_eq!(
                record.character_coding_scheme(),
                CharacterCodingScheme::UcsUnicode
            );
            assert_eq!(record.encoding_level(), EncodingLevel::FullLevel);
            assert_eq!(
                record.descriptive_cataloging_form(),
                DescriptiveCatalogingForm::IsbdPunctuationIncluded
            );
            assert_eq!(
                record.multipart_resource_record_level(),
                MultipartResourceRecordLevel::NotSpecifiedOrNotApplicable
            );
            assert_eq!(record.as_ref(), &RECS.as_bytes()[REC_SIZE as usize..]);
            assert!(records.next().is_none());
            assert!(records.next().is_none());

            let data = &[0x30; 10];
            let input = io::Cursor::new(data);
            let mut records = Records::new(input);
            assert!(records.next().unwrap().is_err());
            assert!(records.next().is_none());
        }

        #[test]
        fn should_get_field() {
            let record = Record::parse(&RECS.as_bytes()[..963]).unwrap();

            let repr = FieldRepr::from((b"001", "000000001"));
            let fields = record.field(b"001");
            assert_eq!(fields, vec![Field::from_repr(&repr)]);

            let repr1 = FieldRepr::from((b"979", "  \x1faautoref"));
            let repr2 = FieldRepr::from((b"979", "  \x1fbautoreg\x1fbautoreh"));
            let fields = record.field(b"979");
            assert_eq!(
                fields,
                vec![Field::from_repr(&repr1), Field::from_repr(&repr2),]
            );

            let fields = record.field(b"999");
            assert_eq!(fields, vec![]);
        }

        #[test]
        fn should_get_fields() {
            let record = Record::parse(&RECS.as_bytes()[..963]).unwrap();

            let tags: Vec<Tag> = record.fields().map(|field| field.get_tag()).collect();
            assert_eq!(
                tags,
                vec![
                    Tag::from(b"001"),
                    Tag::from(b"003"),
                    Tag::from(b"003"),
                    Tag::from(b"005"),
                    Tag::from(b"008"),
                    Tag::from(b"035"),
                    Tag::from(b"040"),
                    Tag::from(b"041"),
                    Tag::from(b"072"),
                    Tag::from(b"100"),
                    Tag::from(b"245"),
                    Tag::from(b"260"),
                    Tag::from(b"300"),
                    Tag::from(b"650"),
                    Tag::from(b"856"),
                    Tag::from(b"979"),
                    Tag::from(b"979"),
                ]
            );
        }

        #[test]
        fn should_build_record() {
            let record = Record::parse(&RECS.as_bytes()[..963]).unwrap();

            let mut builder = RecordBuilder::new();
            builder.add_fields(fields!(
                data fields: [
                    b"979", b"  ", [
                        b'a' => "autoref",
                    ],
                    b"979", b"  ", [
                        b'b' => "autoreg",
                        b'b' => "autoreh",
                    ],
                    b"856", b"41" , [
                        b'q' => "application/pdf",
                        b'u' => "http://dlib.rsl.ru/rsl01000000000/rsl01000000000/rsl01000000001/rsl01000000001.pdf",
                    ],
                    b"650", b" 7", [
                        b'a' => "Всеобщая история",
                        b'2' => "nsnr",
                    ],
                    b"300", b"  ", [
                        b'a' => "24 c.",
                        b'b' => "ил",
                    ],
                    b"260", b"  ", [
                        b'a' => "Санкт-Петербург",
                        b'c' => "1992",
                    ],
                    b"245", b"00", [
                        b'a' => "Этносоциальная структура и институты социальной защиты в Хадрамауте (19 - первая половина 20 вв.) :",
                        b'b' => "автореферат дис. ... кандидата исторических наук : 07.00.03",
                    ],
                    b"100", b"1 ", [
                        b'a' => "'Абд Ал-'Азиз Джа'фар Бин 'Акид",
                    ],
                    b"072", b" 7", [
                        b'a' => "07.00.03",
                        b'2' => "nsnr",
                    ],
                    b"041", b"0 ", [
                        b'a' => "rus",
                    ],
                    b"040", b"  ", [
                        b'a' => "RuMoRGB",
                        b'b' => "rus",
                        b'c' => "RuMoRGB",
                    ],
                    b"035", b"  ", [
                        b'a' => "(RuMoEDL)-92k71098",
                        b'f' => "filter",
                    ],
                ];
                control fields: [
                    b"000" => "filter",
                    b"008" => "080528s1992    ru a|||  a    |00 u rus d",
                    b"005" => "20080528120000.0",
                    b"003" => "RuMoRGB",
                    b"003" => "EnMoRGB",
                    b"001" => "000000001",
                ];
            )).unwrap();
            builder
                .set_record_status(record.record_status())
                .set_type_of_record(record.type_of_record())
                .set_bibliographic_level(record.bibliographic_level())
                .set_type_of_control(record.type_of_control())
                .set_character_coding_scheme(record.character_coding_scheme())
                .set_encoding_level(record.encoding_level())
                .set_descriptive_cataloging_form(record.descriptive_cataloging_form())
                .set_multipart_resource_record_level(record.multipart_resource_record_level());
            builder.filter_fields(|f| f.get_tag() != b"000");
            builder.filter_subfields(|_, sf| sf.get_data::<[u8]>() != &b"filter"[..]);

            assert_eq!(builder.get_record().unwrap().as_ref(), record.as_ref());
        }

        #[test]
        fn should_display_record() {
            let mut builder = RecordBuilder::new();
            builder
                .add_fields(fields!(
                    data fields: [
                        b"264", b" 1", [
                            b'a' => "León, Spain",
                        ],
                        b"245", b"00", [
                            b'a' => "Book title",
                            b'b' => "Book Subtitle",
                        ],
                        b"100", b"1 ", [
                            b'a' => "Author Name",
                        ],
                        b"041", b"0 ", [
                            b'a' => "eng",
                        ],
                    ];
                    control fields: [
                        b"008" => "210128t20212021enka    sb    000 0 eng d",
                        b"001" => "000000001",
                    ];
                ))
                .unwrap();
            let record = builder.get_record().unwrap();

            let expected = "=LDR  00220nam  2200097 i 4500\n=001  000000001\n=008  210128t20212021enka\\\\\\\\sb\\\\\\\\000\\0\\eng\\d\n=041  0 $aeng\n=100  1 $aAuthor Name\n=245  00$aBook title$bBook Subtitle\n=264   1$aLeón, Spain\n".to_string();

            assert_eq!(format!("{}", record), expected);
        }
    }

    mod write {
        use super::{super::*, RECS};

        #[test]
        fn should_write_record() {
            let mut vec = Vec::new();

            let record = Record::parse(&RECS.as_bytes()[..963]).unwrap();

            match vec.write_record(record.clone()) {
                Err(why) => panic!("couldn't write file: {}", why),
                Ok(_) => (),
            }

            let record2 = Record::from_vec(vec).unwrap();
            assert_eq!(record.as_ref(), record2.as_ref());
        }
    }

    #[cfg(feature = "nightly")]
    mod bench {
        use super::{super::*, RECS, REC_SIZE};
        use test;

        #[bench]
        fn read_record(b: &mut test::Bencher) {
            b.iter(|| {
                if let Ok(rec) = Record::read(&mut RECS.as_bytes()) {
                    if let Some(rec) = rec {
                        test::black_box(rec);
                    } else {
                        panic!();
                    }
                }
            });
            b.bytes += REC_SIZE;
        }

        #[bench]
        fn parse_record(b: &mut test::Bencher) {
            b.iter(|| {
                if let Ok(rec) = Record::parse(RECS.as_bytes()) {
                    test::black_box(rec);
                } else {
                    panic!();
                }
            });
            b.bytes += REC_SIZE;
        }
    }
}
