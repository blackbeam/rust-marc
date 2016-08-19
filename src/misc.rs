use errors::*;

pub fn read_dec_1(i: u8) -> Result<u8> {
    if 0x30 > i || i > 0x39 {
        Err(ErrorKind::UnexpectedByteInDecNum(i).into())
    } else {
        Ok(0x0F & i)
    }
}

pub fn read_dec_4(i: &[u8]) -> Result<usize> {
    if i.len() < 4 {
        return Err(ErrorKind::UnexpectedEofInDecNum.into())
    }
    let mut result = 0usize;
    for x in 0..4usize {
        result += 10usize.pow(3 - x as u32) * try!(read_dec_1(i[x])) as usize;
    }
    Ok(result)
}

pub fn read_dec_5(i: &[u8]) -> Result<usize> {
    if i.len() < 5 {
        return Err(ErrorKind::UnexpectedEofInDecNum.into())
    }
    let mut result = 0usize;
    for x in 0..5usize {
        result += 10usize.pow(4 - x as u32) * try!(read_dec_1(i[x])) as usize;
    }
    Ok(result)
}