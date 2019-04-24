use crate::tag::Tag;

/// View into a subfield of a MARC field
pub struct Subfield<'a> {
    tag: Tag,
    identifier: u8,
    data: &'a [u8],
}