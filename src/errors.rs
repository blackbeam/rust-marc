use tag::Tag;

error_chain! {
    errors {
        UnexpectedByteInDecNum(byte: u8) {
            description("Unexpected byte in decimal number")
            display("Unexpected byte in decimal string: {}", byte)
        }
        UnexpectedEofInDecNum {
            description("Unexpected EOF while reading decimal number")
        }
        UnexpectedEof {
            description("Unexpected EOF")
        }
        UnexpectedEofInDirectory {
            description("Unexpected EOF while reading directory")
        }
        NoRecordTerminator {
            description("No record terminator")
        }
        FieldTooLarge(tag: Tag) {
            description("Field byte length is greater than 9998")
            display("Field byte length is greater than 9998 (tag={})", tag)
        }
        RecordTooLarge(size: usize) {
            description("Record byte length is greater than 99999")
            display("Record byte length {} is greater than 99999 limit", size)
        }
        RecordTooShort(len: usize) {
            description("Record length specified in leader is too small")
            display("Record length {} specified in leader is too small", len)
        }
        UnexpectedSubfieldEnd {
            description("Unexpected end of a subfield")
        }
    }
}