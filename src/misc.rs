use crate::errors::*;

pub fn read_dec_1(i: u8) -> Result<u8> {
    if 0x30 > i || i > 0x39 {
        Err(Error::UnexpectedByteInDecNum(i))
    } else {
        Ok(0x0F & i)
    }
}

pub fn read_dec_4(i: &[u8]) -> Result<usize> {
    if i.len() < 4 {
        return Err(Error::UnexpectedEofInDecNum);
    }
    let mut result = 0usize;
    for x in 0..4usize {
        result += 10usize.pow(3 - x as u32) * read_dec_1(i[x])? as usize;
    }
    Ok(result)
}

pub fn read_dec_5(i: &[u8]) -> Result<usize> {
    if i.len() < 5 {
        return Err(Error::UnexpectedEofInDecNum);
    }
    let mut result = 0usize;
    for x in 0..5usize {
        result += 10usize.pow(4 - x as u32) * read_dec_1(i[x])? as usize;
    }
    Ok(result)
}
