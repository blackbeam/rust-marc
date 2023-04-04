pub mod fields;
pub mod subfield;

use std::{fmt, str};

use self::subfield::{subfields::Subfields, Subfield};
use crate::{errors::*, Identifier, Indicator, Tag, MAX_FIELD_LEN, SUBFIELD_DELIMITER};
/// View into a field of a MARC record
#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Field<'a> {
    tag: Tag,
    data: &'a [u8],
}

impl<'a> Field<'a> {
    #[doc(hidden)]
    pub fn new(tag: Tag, data: &[u8]) -> Field<'_> {
        Field { tag, data }
    }

    /// Will find all subfields with identifier `ident`.
    pub fn subfield<Ident: Into<Identifier>>(&self, ident: Ident) -> Vec<Subfield<'_>> {
        Subfield::find(self, ident)
    }

    /// Will return iterator over subfields of the field.
    pub fn subfields(&self) -> Subfields<'a> {
        Subfields::new(self.clone())
    }

    /// Will return view into a `FieldRepr`.
    pub fn from_repr(repr: &'a FieldRepr) -> Field<'a> {
        Field {
            tag: repr.tag,
            data: &*repr.data,
        }
    }

    /// Returns tag of the field.
    pub fn get_tag(&self) -> Tag {
        self.tag
    }

    /// Returns data of the field.
    ///
    /// Data will not include field terminator.
    pub fn get_data<O: FromFieldData + ?Sized>(&self) -> &O {
        FromFieldData::from_data(self.data)
    }
}

impl fmt::Display for Field<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tag = self.get_tag();
        let field_data = match tag.0 {
            [b'0', b'0', ..] => {
                // variable control field
                self.get_data::<str>().replace(' ', "\\")
            }
            _ => {
                // variable data field
                self.get_data::<[u8]>()
                    .iter()
                    .map(|&b| {
                        if b == SUBFIELD_DELIMITER {
                            '$'
                        } else {
                            b as char
                        }
                    })
                    .collect::<String>()
            }
        };
        write!(f, "{}  {}", tag, field_data)
    }
}

/// MARC field representation
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct FieldRepr {
    tag: Tag,
    data: Vec<u8>,
}

impl FieldRepr {
    /// Will return new `FieldRepr` with specified subfield included.
    ///
    /// ### Errors
    /// Will return Error if resuling field length (with field terminator) is greater than
    /// 9.999 bytes.
    pub fn add_subfield<Ident, D>(&self, identifier: Ident, f_data: D) -> Result<FieldRepr>
    where
        Ident: Into<Identifier>,
        D: AsRef<[u8]>,
    {
        let mut new_data = self.data.clone();
        new_data.push(SUBFIELD_DELIMITER);
        new_data.push(identifier.into().into());
        new_data.extend_from_slice(f_data.as_ref());
        if new_data.len() + 1 > MAX_FIELD_LEN {
            return Err(Error::FieldTooLarge(self.tag));
        }
        Ok(FieldRepr {
            tag: self.tag,
            data: new_data,
        })
    }

    /// Will return new `FieldRepr` filtered by `fun`.
    ///
    /// Subfield will be removed if `fun` returns `false` on it.
    pub fn filter_subfields<F>(&self, mut fun: F) -> FieldRepr
    where
        F: FnMut(&subfield::Subfield<'_>) -> bool,
    {
        if let Some(&SUBFIELD_DELIMITER) = self.data.get(2) {
            let mut new_data = vec![];
            new_data.extend_from_slice(&self.data[0..2]);

            let f = Field::from_repr(&self);
            let sfs = f
                .subfields()
                .filter(|ref sf| fun(sf))
                .collect::<Vec<subfield::Subfield<'_>>>();

            for sf in sfs {
                new_data.push(SUBFIELD_DELIMITER);
                new_data.push(sf.get_identifier().0);
                new_data.extend_from_slice(sf.get_data());
            }

            FieldRepr {
                tag: self.tag,
                data: new_data,
            }
        } else {
            self.clone()
        }
    }

    /// Returns tag of a field.
    pub fn get_tag(&self) -> Tag {
        self.tag
    }

    /// Returns data of a field (no field terminator).
    pub fn get_data(&self) -> &[u8] {
        &*self.data
    }
}

impl fmt::Debug for FieldRepr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl fmt::Display for FieldRepr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Field({} Data({}))",
            self.tag,
            str::from_utf8(&*self.data).unwrap()
        )
    }
}

impl<T, Ind, Ident, D> From<(T, Ind, Vec<(Ident, D)>)> for FieldRepr
where
    T: Into<Tag>,
    Ind: Into<Indicator>,
    Ident: Into<Identifier>,
    D: Into<Vec<u8>>,
{
    fn from((tag, indicator, subfields): (T, Ind, Vec<(Ident, D)>)) -> FieldRepr {
        let mut repr = FieldRepr::from((tag, indicator.into().as_ref()));
        for (identifier, data) in subfields {
            repr = repr.add_subfield(identifier, data.into()).unwrap();
        }
        repr
    }
}

impl<T: Into<Tag>, D: Into<Vec<u8>>> From<(T, D)> for FieldRepr {
    fn from((tag, data): (T, D)) -> FieldRepr {
        FieldRepr {
            tag: tag.into(),
            data: data.into(),
        }
    }
}

impl<'a> From<Field<'a>> for FieldRepr {
    fn from(f: Field<'a>) -> FieldRepr {
        FieldRepr::from((f.get_tag(), f.get_data::<[u8]>().to_vec()))
    }
}

pub trait FromFieldData {
    fn from_data(data: &[u8]) -> &Self;
}

impl FromFieldData for [u8] {
    fn from_data(data: &[u8]) -> &[u8] {
        data
    }
}

impl FromFieldData for str {
    fn from_data(data: &[u8]) -> &str {
        str::from_utf8(data).unwrap()
    }
}

#[cfg(test)]
mod test {
    use crate::field::FieldRepr;

    #[test]
    fn should_filter_subfields() {
        let repr: FieldRepr = FieldRepr::from((b"979", "  \x1fbautoreg\x1fbautoreh"));
        let f1 = repr.filter_subfields(|_| false);
        let mut i = 0;
        let f2 = repr.filter_subfields(|_| {
            i += 1;
            i == 1
        });
        let f3 = repr.filter_subfields(|_| true);

        assert_eq!(f1, FieldRepr::from((b"979", "  ")));
        assert_eq!(f2, FieldRepr::from((b"979", "  \x1fbautoreg")));
        assert_eq!(f3, FieldRepr::from((b"979", "  \x1fbautoreg\x1fbautoreh")));
    }

    #[test]
    fn should_add_subfield() {
        let repr: FieldRepr = FieldRepr::from((b"979", "  \x1fbautoreg"));
        let repr2 = repr.add_subfield(b'b', "autoreh").unwrap();
        assert_eq!(
            repr2,
            FieldRepr::from((b"979", "  \x1fbautoreg\x1fbautoreh"))
        );
    }

    #[test]
    #[should_panic]
    fn should_panic_if_field_is_too_large() {
        let repr: FieldRepr = FieldRepr::from((b"979", "  "));
        repr.add_subfield(b'a', vec![b'x'; 9995]).unwrap();
    }
}
