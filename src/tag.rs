use std::{fmt, str};

/// Tag of a field
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Tag(pub [u8; 3]);

impl Tag {
    /// Creates a tag from the given slice.
    ///
    /// # Panic
    ///
    /// Will panic if `bytes.len() != 3`.
    pub fn from_slice(bytes: &[u8]) -> Self {
        let mut this = Tag([0; 3]);
        this.0.copy_from_slice(bytes);
        this
    }
}

impl PartialEq<[u8; 3]> for Tag {
    fn eq(&self, other: &[u8; 3]) -> bool {
        self.0 == *other
    }
}

impl PartialEq<&'_ [u8; 3]> for Tag {
    fn eq(&self, other: &&'_ [u8; 3]) -> bool {
        &self.0 == *other
    }
}

impl PartialEq<[u8]> for Tag {
    fn eq(&self, other: &[u8]) -> bool {
        &self.0[..] == other
    }
}

impl PartialEq<&'_ [u8]> for Tag {
    fn eq(&self, other: &&'_ [u8]) -> bool {
        &self.0[..] == *other
    }
}

impl PartialEq<str> for Tag {
    fn eq(&self, other: &str) -> bool {
        &self.0[..] == other.as_bytes()
    }
}

impl PartialEq<&'_ str> for Tag {
    fn eq(&self, other: &&'_ str) -> bool {
        &self.0[..] == other.as_bytes()
    }
}

impl PartialEq<Tag> for [u8; 3] {
    fn eq(&self, other: &Tag) -> bool {
        *other == *self
    }
}

impl PartialEq<Tag> for &'_ [u8; 3] {
    fn eq(&self, other: &Tag) -> bool {
        *other == *self
    }
}

impl PartialEq<Tag> for [u8] {
    fn eq(&self, other: &Tag) -> bool {
        *other == *self
    }
}

impl PartialEq<Tag> for &'_ [u8] {
    fn eq(&self, other: &Tag) -> bool {
        *other == *self
    }
}

impl PartialEq<Tag> for str {
    fn eq(&self, other: &Tag) -> bool {
        *other == *self
    }
}

impl PartialEq<Tag> for &'_ str {
    fn eq(&self, other: &Tag) -> bool {
        *other == *self
    }
}

impl AsRef<[u8]> for Tag {
    fn as_ref(&self) -> &[u8] {
        &self.0[..]
    }
}

impl fmt::Debug for Tag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl fmt::Display for Tag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Tag({})", String::from_utf8_lossy(&self.0))
    }
}

impl From<[u8; 3]> for Tag {
    fn from(s: [u8; 3]) -> Tag {
        Tag(s)
    }
}

impl From<&'_ [u8; 3]> for Tag {
    fn from(s: &'_ [u8; 3]) -> Tag {
        Tag(*s)
    }
}
