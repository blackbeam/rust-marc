use crate::field::Field;
use crate::Record;

/// Iterator over fields of a record.
pub struct Fields<'a> {
    record: &'a Record<'a>,
    offset: usize,
}

impl<'a> Fields<'a> {
    #[doc(hidden)]
    pub fn new(record: &'a Record<'a>) -> Fields<'a> {
        Fields {
            record: record,
            offset: 0,
        }
    }
}

impl<'a> Iterator for Fields<'a> {
    type Item = Field<'a>;

    fn next(&mut self) -> Option<Field<'a>> {
        let i = self.offset;
        self.offset += 1;
        self.record.directory.entries.get(i).map(|entry| {
            let offset = self.record.data_offset + entry.2;
            Field::new(entry.0, &self.record.data[offset..offset + entry.1 - 1])
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let bound = self.record.directory.entries.len() - self.offset;
        (bound, Some(bound))
    }
}