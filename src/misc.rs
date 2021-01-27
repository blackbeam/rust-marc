use crate::errors::*;

pub fn read_dec_1(i: u8) -> Result<u8> {
    if !(0x30..=0x39).contains(&i) {
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
    for (x, byte) in i.iter().enumerate().take(4) {
        result += 10_usize.pow(3 - x as u32) * read_dec_1(*byte)? as usize;
    }
    Ok(result)
}

pub fn read_dec_5(i: &[u8]) -> Result<usize> {
    if i.len() < 5 {
        return Err(Error::UnexpectedEofInDecNum);
    }
    let mut result = 0usize;
    for (x, byte) in i.iter().enumerate().take(5) {
        result += 10_usize.pow(4 - x as u32) * read_dec_1(*byte)? as usize;
    }
    Ok(result)
}
