use std::{fmt, str};

/// Tag of a field
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Tag(pub [u8; 3]);

impl AsRef<[u8]> for Tag {
    fn as_ref(&self) -> &[u8] {
        &self.0[..]
    }
}

impl fmt::Debug for Tag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl fmt::Display for Tag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Tag({})", str::from_utf8(&self.0[..]).unwrap())
    }
}

impl<'a> From<&'a [u8]> for Tag {
    #[inline]
    fn from(s: &'a [u8]) -> Tag {
        assert!(s.len() == 3, "Tag length != 3");
        Tag([s[0], s[1], s[2]])
    }
}

impl<'a> From<&'a str> for Tag {
    #[inline]
    fn from(s: &'a str) -> Tag {
        s.as_bytes().into()
    }
}

impl<'a> From<&'a [u8; 3]> for Tag {
    #[inline]
    fn from(s: &'a [u8; 3]) -> Tag {
        Tag::from(*s)
    }
}

impl From<[u8; 3]> for Tag {
    #[inline]
    fn from(s: [u8; 3]) -> Tag {
        Tag(s)
    }
}

impl<'a> From<&'a Tag> for &'a str {
    #[inline]
    fn from(tag: &'a Tag) -> &'a str {
        str::from_utf8(&tag.0[..]).unwrap()
    }
}

impl<'a> From<&'a Tag> for &'a [u8] {
    #[inline]
    fn from(tag: &'a Tag) -> &'a [u8] {
        &tag.0[..]
    }
}

impl<'a> From<&'a Tag> for [u8; 3] {
    #[inline]
    fn from(tag: &'a Tag) -> [u8; 3] {
        tag.0
    }
}
