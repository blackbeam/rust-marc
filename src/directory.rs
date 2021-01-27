use crate::{errors::*, misc, tag::Tag, FIELD_TERMINATOR};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Directory {
    pub entries: Vec<(Tag, usize, usize)>,
}

impl Directory {
    pub fn parse(input: &[u8]) -> Result<Directory> {
        let mut entries = Vec::with_capacity(16);
        for chunk in input.chunks(12) {
            if chunk.len() == 12 {
                let tag = Tag::from_slice(&chunk[0..3]);
                let len = misc::read_dec_4(&chunk[3..7])?;
                let offset = misc::read_dec_5(&chunk[7..12])?;
                entries.push((tag, len, offset));
            } else if chunk.len() == 1 && chunk[0] == FIELD_TERMINATOR {
                break;
            } else {
                return Err(Error::UnexpectedEofInDirectory);
            }
        }
        Ok(Directory { entries })
    }
}

#[cfg(test)]
mod test {
    use crate::Tag;

    use super::Directory;

    #[test]
    fn should_parse_directory() {
        let data = b"001001000000\
                     003000800010\
                     003000800018\
                     005001700026\
                     008004100043\x1e"
            .to_vec();
        let dir = Directory::parse(&*data).unwrap();
        assert_eq!(
            vec! {
                (Tag::from(b"001"), 10usize,  0usize),
                (Tag::from(b"003"),  8, 10),
                (Tag::from(b"003"),  8, 18),
                (Tag::from(b"005"), 17, 26),
                (Tag::from(b"008"), 41, 43),
            },
            dir.entries,
        );
    }

    #[test]
    fn should_parse_empty_directory() {
        let data = b"\x1e".to_vec();
        let dir = Directory::parse(&*data).unwrap();
        assert_eq!(dir.entries, vec![]);
    }
}
