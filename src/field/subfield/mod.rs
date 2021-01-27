use crate::Identifier;

pub mod subfields;

use crate::{
    field::{Field, FromFieldData},
    tag::Tag,
};

/// View into a subfield of a MARC field
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Subfield<'a> {
    tag: Tag,
    identifier: Identifier,
    data: &'a [u8],
}

impl<'a> Subfield<'a> {
    #[doc(hidden)]
    pub fn find<'f, 'r, Ident>(f: &'r Field<'f>, identifier: Ident) -> Vec<Subfield<'r>>
    where
        Ident: Into<Identifier>,
    {
        let mut output = Vec::with_capacity(4);
        let identifier = identifier.into();
        for sf in f.subfields() {
            if sf.identifier == identifier {
                output.push(sf);
            }
        }
        output
    }

    /// Returns tag of a field this subfield belongs to.
    pub fn get_tag(&self) -> Tag {
        self.tag
    }

    /// Returns identifier of this subfield
    pub fn get_identifier(&self) -> Identifier {
        self.identifier
    }

    /// Returns data of this subfield without subfield delimiter or identifier.
    pub fn get_data<O: FromFieldData + ?Sized>(&self) -> &O {
        FromFieldData::from_data(self.data)
    }
}

#[cfg(test)]
mod test {
    use crate::field::{Field, FieldRepr};

    #[test]
    fn should_find_subfields() {
        let field_repr1: FieldRepr = FieldRepr::from(("979", "  \x1faa\x1fbb\x1fbc\x1e"));
        let field_repr3: FieldRepr = FieldRepr::from(("001", "1\x1e"));

        let field1 = Field::from_repr(&field_repr1);
        let field3 = Field::from_repr(&field_repr3);

        let subfields1 = field1.subfield(b'a');
        let subfields2 = field1.subfield(b'b');
        let subfields3 = field1.subfield(b'c');
        let subfields4 = field3.subfield(b'a');

        assert_eq!(subfields1.len(), 1);
        assert_eq!(subfields2.len(), 2);
        assert_eq!(subfields3.len(), 0);
        assert_eq!(subfields4.len(), 0);
    }
}
