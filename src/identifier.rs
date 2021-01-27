/// Subfield identifier
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Identifier(pub u8);

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
        &[self.0] == other
    }
}

impl PartialEq<&'_ [u8]> for Identifier {
    fn eq(&self, other: &&'_ [u8]) -> bool {
        &[self.0] == *other
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
