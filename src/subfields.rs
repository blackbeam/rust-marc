use crate::field::Field;
use crate::subfield::Subfield;

pub struct Subfields<'a> {
    field: Field<'a>,
    offset: usize,
    start: Option<usize>,
    ident: Option<u8>,
    count: Option<u32>,
}

impl<'a> Subfields<'a> {
    pub fn new(f: Field<'a>) -> Subfields<'a> {
        Subfields {
            field: f,
            offset: 0,
            start: None,
            ident: None,
            count: None,
        }
    }
}

impl<'a> Iterator for Subfields<'a> {
    type Item = Subfield<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        unimplemented!()
    }
}