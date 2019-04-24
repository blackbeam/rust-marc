use crate::field::Field;
use crate::field::subfield::Subfield;
use crate::SUBFIELD_DELIMITER;

/// Iterator over subfields of a field.
#[derive(Eq, PartialEq, Clone)]
pub struct Subfields<'a> {
    field: Field<'a>,
    state: State,
}

impl<'a> Subfields<'a> {
    pub fn new(f: Field<'a>) -> Subfields<'a> {
        Subfields {
            field: f,
            state: State::Initial,
        }
    }
}

impl<'a> Iterator for Subfields<'a> {
    type Item = Subfield<'a>;

    fn next(&mut self) -> Option<Subfield<'a>> {
        match self.state {
            State::Initial => {
                // Jump over indicator
                self.state = State::Start(2);
                self.next()
            },
            State::Start(offset) => {
                match self.field.data.get(offset).map(|x| *x) {
                    Some(SUBFIELD_DELIMITER) => {
                        self.state = State::SubfieldStart(offset + 1);
                        self.next()
                    },
                    _ => {
                        self.state = State::Done;
                        self.next()
                    },
                }
            },
            State::SubfieldStart(offset) => {
                match self.field.data.get(offset).map(|x| *x) {
                    Some(SUBFIELD_DELIMITER) | None => {
                        // Subfield ends unexpectedly
                        self.state = State::Done;
                        None
                    },
                    Some(byte) => {
                        self.state = State::Subfield(byte, offset + 1, 0);
                        self.next()
                    },
                }
            },
            State::Subfield(identifier, start, offset) => {
                match self.field.data.get(start + offset).map(|x| *x) {
                    Some(SUBFIELD_DELIMITER) => {
                        self.state = State::SubfieldStart(start + offset + 1);
                        Some(Subfield {
                            tag: self.field.tag,
                            identifier: identifier.into(),
                            data: &self.field.data[start..start + offset],
                        })
                    },
                    None => {
                        self.state = State::Done;
                        Some(Subfield {
                            tag: self.field.tag,
                            identifier: identifier.into(),
                            data: &self.field.data[start..start + offset],
                        })
                    },
                    Some(_) => {
                        self.state = State::Subfield(identifier, start, offset + 1);
                        self.next()
                    }
                }
            },
            State::Done => None,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum State {
    Initial,
    Start(usize),
    SubfieldStart(usize),
    Subfield(u8, usize, usize),
    Done,
}

#[cfg(test)]
mod test {
    use crate::field::{Field, FieldRepr};

    #[test]
    fn should_iterate_subfields() {
        let field_repr1: FieldRepr = FieldRepr::from(("979", "  \x1fbautoreg\x1fbautoreh"));
        let field_repr2: FieldRepr = FieldRepr::from(("979", "  "));
        let field_repr3: FieldRepr = FieldRepr::from(("001", "1"));

        let field1 = Field::from_repr(&field_repr1);
        let field2 = Field::from_repr(&field_repr2);
        let field3 = Field::from_repr(&field_repr3);

        let subfields1: Vec<_> = field1.subfields().collect();
        let subfields2: Vec<_> = field2.subfields().collect();
        let subfields3: Vec<_> = field3.subfields().collect();

        assert_eq!(subfields1.len(), 2);
        assert_eq!(subfields2.len(), 0);
        assert_eq!(subfields3.len(), 0);
    }
}