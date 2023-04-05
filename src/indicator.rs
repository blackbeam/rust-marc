/// Variable data field indicator.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Indicator(pub [u8; 2]);

impl Indicator {
    /// Creates an indicator from the given slice.
    ///
    /// # Painc
    ///
    /// Will panic if `bytes.len() != 2`.
    pub fn from_slice(bytes: &[u8]) -> Self {
        Indicator(bytes.try_into().expect("indicator size mismatch"))
    }

    /// Returns the first indicator as `str`.
    ///
    /// # Panic
    ///
    /// Will panic in case of non-utf8 indicator.
    pub fn first(&self) -> &str {
        std::str::from_utf8(&self.0[..1]).expect("non-utf8 indicator")
    }

    /// Returns the second indicator as `str`.
    ///
    /// # Panic
    ///
    /// Will panic in case of non-utf8 indicator.
    pub fn second(&self) -> &str {
        std::str::from_utf8(&self.0[..1]).expect("non-utf8 indicator")
    }
}

impl AsRef<[u8]> for Indicator {
    fn as_ref(&self) -> &[u8] {
        &self.0[..]
    }
}

impl From<[u8; 2]> for Indicator {
    fn from(s: [u8; 2]) -> Indicator {
        Indicator(s)
    }
}

impl From<&'_ [u8; 2]> for Indicator {
    fn from(s: &'_ [u8; 2]) -> Indicator {
        Indicator(*s)
    }
}

impl PartialEq<[u8; 2]> for Indicator {
    fn eq(&self, other: &[u8; 2]) -> bool {
        self.0 == *other
    }
}

impl PartialEq<&'_ [u8; 2]> for Indicator {
    fn eq(&self, other: &&'_ [u8; 2]) -> bool {
        &self.0 == *other
    }
}

impl PartialEq<[u8]> for Indicator {
    fn eq(&self, other: &[u8]) -> bool {
        &self.0[..] == other
    }
}

impl PartialEq<&'_ [u8]> for Indicator {
    fn eq(&self, other: &&'_ [u8]) -> bool {
        &self.0[..] == *other
    }
}

impl PartialEq<str> for Indicator {
    fn eq(&self, other: &str) -> bool {
        &self.0[..] == other.as_bytes()
    }
}

impl PartialEq<&'_ str> for Indicator {
    fn eq(&self, other: &&'_ str) -> bool {
        &self.0[..] == other.as_bytes()
    }
}

impl PartialEq<Indicator> for [u8; 2] {
    fn eq(&self, other: &Indicator) -> bool {
        *other == *self
    }
}

impl PartialEq<Indicator> for &'_ [u8; 2] {
    fn eq(&self, other: &Indicator) -> bool {
        *other == *self
    }
}

impl PartialEq<Indicator> for [u8] {
    fn eq(&self, other: &Indicator) -> bool {
        *other == *self
    }
}

impl PartialEq<Indicator> for &'_ [u8] {
    fn eq(&self, other: &Indicator) -> bool {
        *other == *self
    }
}

impl PartialEq<Indicator> for str {
    fn eq(&self, other: &Indicator) -> bool {
        *other == *self
    }
}

impl PartialEq<Indicator> for &'_ str {
    fn eq(&self, other: &Indicator) -> bool {
        *other == *self
    }
}
