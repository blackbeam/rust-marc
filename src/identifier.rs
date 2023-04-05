use std::fmt;

/// Subfield identifier
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Identifier(pub u8);

impl Identifier {
    /// Returns identifier as a `char`.
    ///
    /// # Panic
    ///
    /// Will panic in case of non-utf8 identifier.
    pub fn as_char(&self) -> char {
        std::str::from_utf8(&[self.0])
            .expect("non-utf8 identifier")
            .chars()
            .next()
            .unwrap()
    }
}

impl From<Identifier> for u8 {
    fn from(x: Identifier) -> Self {
        x.0
    }
}

impl From<u8> for Identifier {
    fn from(x: u8) -> Identifier {
        Identifier(x)
    }
}

impl From<&'_ u8> for Identifier {
    fn from(x: &'_ u8) -> Identifier {
        Identifier(*x)
    }
}

impl From<[u8; 1]> for Identifier {
    fn from(s: [u8; 1]) -> Identifier {
        Identifier::from(s[0])
    }
}

impl From<&'_ [u8; 1]> for Identifier {
    fn from(s: &'_ [u8; 1]) -> Identifier {
        Identifier::from(*s)
    }
}

impl PartialEq<u8> for Identifier {
    fn eq(&self, other: &u8) -> bool {
        self.0 == *other
    }
}

impl PartialEq<&'_ u8> for Identifier {
    fn eq(&self, other: &&'_ u8) -> bool {
        self.0 == **other
    }
}

impl PartialEq<[u8]> for Identifier {
    fn eq(&self, other: &[u8]) -> bool {
        [self.0] == other
    }
}

impl PartialEq<&'_ [u8]> for Identifier {
    fn eq(&self, other: &&'_ [u8]) -> bool {
        [self.0] == *other
    }
}

impl PartialEq<char> for Identifier {
    fn eq(&self, other: &char) -> bool {
        let mut buf = [0; 4];
        self == other.encode_utf8(&mut buf).as_bytes()
    }
}

impl PartialEq<&'_ char> for Identifier {
    fn eq(&self, other: &&'_ char) -> bool {
        *self == **other
    }
}

impl PartialEq<str> for Identifier {
    fn eq(&self, other: &str) -> bool {
        self == other.as_bytes()
    }
}

impl PartialEq<&'_ str> for Identifier {
    fn eq(&self, other: &&'_ str) -> bool {
        self == other.as_bytes()
    }
}

impl PartialEq<Identifier> for u8 {
    fn eq(&self, other: &Identifier) -> bool {
        *other == *self
    }
}

impl PartialEq<Identifier> for &'_ u8 {
    fn eq(&self, other: &Identifier) -> bool {
        *other == *self
    }
}

impl PartialEq<Identifier> for [u8] {
    fn eq(&self, other: &Identifier) -> bool {
        *other == *self
    }
}

impl PartialEq<Identifier> for &'_ [u8] {
    fn eq(&self, other: &Identifier) -> bool {
        *other == *self
    }
}

impl PartialEq<Identifier> for char {
    fn eq(&self, other: &Identifier) -> bool {
        *other == *self
    }
}

impl PartialEq<Identifier> for &'_ char {
    fn eq(&self, other: &Identifier) -> bool {
        *other == *self
    }
}

impl PartialEq<Identifier> for str {
    fn eq(&self, other: &Identifier) -> bool {
        *other == *self
    }
}

impl PartialEq<Identifier> for &'_ str {
    fn eq(&self, other: &Identifier) -> bool {
        *other == *self
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", String::from_utf8_lossy(&[self.0]))
    }
}
