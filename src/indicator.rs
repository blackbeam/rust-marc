/// Variable data field indicator.
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Indicator(pub [u8; 2]);

impl Indicator {
    pub fn as_ref(&self) -> &[u8] {
        &self.0[..]
    }
}

impl<'a> From<&'a [u8]> for Indicator {
    #[inline]
    fn from(s: &'a [u8]) -> Indicator {
        assert!(s.len() == 2, "Variable data field indicator length != 2");
        Indicator([s[0], s[1]])
    }
}

impl<'a> From<&'a str> for Indicator {
    #[inline]
    fn from(s: &'a str) -> Indicator {
        s.as_bytes().into()
    }
}

impl<'a> From<&'a [u8; 2]> for Indicator {
    #[inline]
    fn from(s: &'a [u8; 2]) -> Indicator {
        Indicator::from(*s)
    }
}

impl From<[u8; 2]> for Indicator {
    #[inline]
    fn from(s: [u8; 2]) -> Indicator {
        Indicator(s)
    }
}
