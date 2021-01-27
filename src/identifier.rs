/// Subfield identifier
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Identifier(pub u8);

impl From<Identifier> for u8 {
    fn from(x: Identifier) -> Self {
        x.0
    }
}

impl<'a> From<&'a [u8]> for Identifier {
    #[inline]
    fn from(s: &'a [u8]) -> Identifier {
        assert!(s.len() == 1, "Subfield identifier length != 1");
        Identifier(s[0])
    }
}

impl<'a> From<&'a str> for Identifier {
    #[inline]
    fn from(s: &'a str) -> Identifier {
        s.as_bytes().into()
    }
}

impl<'a> From<&'a [u8; 1]> for Identifier {
    #[inline]
    fn from(s: &'a [u8; 1]) -> Identifier {
        Identifier::from(*s)
    }
}

impl From<[u8; 1]> for Identifier {
    #[inline]
    fn from(s: [u8; 1]) -> Identifier {
        Identifier(s[0])
    }
}

impl From<char> for Identifier {
    #[inline]
    fn from(c: char) -> Identifier {
        let mut s = String::with_capacity(1);
        s.push(c);
        Identifier::from(s.as_bytes())
    }
}

impl From<u8> for Identifier {
    #[inline]
    fn from(c: u8) -> Identifier {
        Identifier(c)
    }
}
