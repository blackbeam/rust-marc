//! # Module to convert MARC 21 records to MARC XML

use crate::{Error, Field, Record, Result, Subfield};
use std::io::Write;
use xml::writer::{EmitterConfig, EventWriter, XmlEvent};

const MARCXML_NS: &[(&str, &str)] = &[
    ("xmlns:marc", "http://www.loc.gov/MARC21/slim"),
    ("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance"),
    (
        "xsi:schemaLocation",
        "http://www.loc.gov/MARC21/slim http://www.loc.gov/standards/marcxml/schema/MARC21slim.xsd",
    ),
];

pub trait XmlElement {
    fn xml_element<W: Write>(&self, w: &mut EventWriter<W>) -> Result<()>;
}

pub trait XmlRootElement<T: XmlElement> {
    fn xml_root_element<W: Write>(&self, w: &mut EventWriter<W>) -> Result<()>;
}

/// Output a single record or a collection of records as MARC XML.
///
/// ## Examples
///
/// ### Outputting a single record
///
/// ```rust
/// # use marc::*;
/// # fn main() -> Result<()> {
/// let mut builder = RecordBuilder::new();
/// let record = builder
///     .add_fields(fields!(
///         control fields: [
///             b"001" => "000000002",
///             b"003" => "RuMoRGB",
///         ];
///         data fields: [
///             b"979", b"  ", [
///                 b'a' => "autoref",
///                 b'a' => "dlopen",
///             ],
///         ];
///     ))?
///     .get_record()?;
/// assert_eq!(String::from_utf8(record.xml_minified()?).unwrap(), "<?xml version=\"1.0\" encoding=\
/// \"utf-8\"?><marc:record xmlns:marc=\"http://www.loc.gov/MARC21/slim\" xmlns:xsi=\"http://www.w3\
/// .org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.loc.gov/MARC21/slim http://www.l\
/// oc.gov/standards/marcxml/schema/MARC21slim.xsd\"><marc:leader>00100nam  2200061 i 4500</marc:le\
/// ader><marc:controlfield tag=\"001\">000000002</marc:controlfield><marc:controlfield tag=\"003\"\
/// >RuMoRGB</marc:controlfield><marc:datafield tag=\"979\" ind1=\" \" ind2=\" \"><marc:subfield co\
/// de=\"a\">autoref</marc:subfield><marc:subfield code=\"a\">dlopen</marc:subfield></marc:datafiel\
/// d></marc:record>".to_string());
/// # Ok(())
/// # }
/// ```
///
/// ### Outputting a collection of records
///
/// ```rust
/// # use marc::*;
/// # fn main() -> Result<()> {
/// let mut builder = RecordBuilder::new();
/// let records = vec![builder
///     .add_fields(fields!(
///         control fields: [
///             b"001" => "000000002",
///             b"003" => "RuMoRGB",
///         ];
///         data fields: [
///             b"979", b"  ", [
///                 b'a' => "autoref",
///                 b'a' => "dlopen",
///             ],
///         ];
///     ))?
///     .get_record()?];
/// assert_eq!(String::from_utf8(records.xml_minified()?).unwrap(), "<?xml version=\"1.0\" enc\
/// oding=\"utf-8\"?><marc:collection xmlns:marc=\"http://www.loc.gov/MARC21/slim\" xmlns:xsi=\"htt\
/// p://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.loc.gov/MARC21/slim ht\
/// tp://www.loc.gov/standards/marcxml/schema/MARC21slim.xsd\"><marc:record><marc:leader>00100nam  \
/// 2200061 i 4500</marc:leader><marc:controlfield tag=\"001\">000000002</marc:controlfield><marc:c\
/// ontrolfield tag=\"003\">RuMoRGB</marc:controlfield><marc:datafield tag=\"979\" ind1=\" \" ind2=\
/// \" \"><marc:subfield code=\"a\">autoref</marc:subfield><marc:subfield code=\"a\">dlopen</marc:s\
/// ubfield></marc:datafield></marc:record></marc:collection>".to_string());
/// # Ok(())
/// # }
/// ```
#[cfg_attr(docsrs, doc(cfg(feature = "xml")))]
pub trait MarcXml<'a>
where
    Self: XmlRootElement<Record<'a>>,
{
    /// Output MARC XML
    fn xml(&self, pretty_print: bool) -> Result<Vec<u8>> {
        let mut buffer = Vec::new();
        let mut writer = EmitterConfig::new()
            .perform_indent(pretty_print)
            .create_writer(&mut buffer);

        self.xml_root_element(&mut writer)?;
        Ok(buffer)
    }

    /// Output minified (outdented) MARC XML
    fn xml_minified(&self) -> Result<Vec<u8>> {
        Self::xml(self, false)
    }

    /// Output pretty-print (indented) MARC XML
    fn xml_pretty(&self) -> Result<Vec<u8>> {
        Self::xml(self, true)
    }
}

impl<'a> MarcXml<'a> for Vec<Record<'a>> {}
impl<'a> MarcXml<'a> for Record<'a> {}

impl XmlRootElement<Record<'_>> for Vec<Record<'_>> {
    fn xml_root_element<W: Write>(&self, w: &mut EventWriter<W>) -> Result<()> {
        write_element("marc:collection", MARCXML_NS.to_vec(), w, |w| {
            for record in self {
                record.xml_element(w)?;
            }
            Ok(())
        })?;
        Ok(())
    }
}

impl XmlRootElement<Record<'_>> for Record<'_> {
    fn xml_root_element<W: Write>(&self, w: &mut EventWriter<W>) -> Result<()> {
        write_element("marc:record", MARCXML_NS.to_vec(), w, |w| {
            write_element("marc:leader", vec![], w, |w| {
                w.write(XmlEvent::Characters(&String::from_utf8_lossy(
                    &self.as_ref()[0..24],
                )))
                .map_err(Into::into)
            })?;
            for field in self.fields() {
                field.xml_element(w)?;
            }
            Ok(())
        })
    }
}

impl XmlElement for Record<'_> {
    fn xml_element<W: Write>(&self, w: &mut EventWriter<W>) -> Result<()> {
        write_element("marc:record", vec![], w, |w| {
            write_element("marc:leader", vec![], w, |w| {
                w.write(XmlEvent::Characters(&String::from_utf8_lossy(
                    &self.as_ref()[0..24],
                )))
                .map_err(Into::into)
            })?;
            for field in self.fields() {
                field.xml_element(w)?;
            }
            Ok(())
        })
    }
}

impl XmlElement for Field<'_> {
    fn xml_element<W: Write>(&self, w: &mut EventWriter<W>) -> Result<()> {
        let tag = self.get_tag();
        match tag.0 {
            [b'0', b'0', ..] => {
                let attributes = vec![("tag", tag.as_str())];
                write_element("marc:controlfield", attributes, w, |w| {
                    w.write(XmlEvent::Characters(self.get_data::<str>()))
                        .map_err(Into::into)
                })?;
            }
            _ => {
                let indicator = self.get_indicator();
                let attributes = vec![
                    ("tag", tag.as_str()),
                    ("ind1", indicator.first()),
                    ("ind2", indicator.second()),
                ];
                write_element("marc:datafield", attributes, w, |w| {
                    for subfield in self.subfields() {
                        subfield.xml_element(w)?;
                    }
                    Ok(())
                })?;
            }
        }
        Ok(())
    }
}

impl XmlElement for Subfield<'_> {
    fn xml_element<W: Write>(&self, w: &mut EventWriter<W>) -> Result<()> {
        let code: &str = &self.get_identifier().as_char().to_string();
        let attributes = vec![("code", code)];
        write_element("marc:subfield", attributes, w, |w| {
            w.write(XmlEvent::Characters(self.get_data::<str>()))
                .map_err(Into::into)
        })?;
        Ok(())
    }
}

fn write_element<W: Write, F: Fn(&mut EventWriter<W>) -> Result<()>>(
    element: &str,
    attr: Vec<(&str, &str)>,
    w: &mut EventWriter<W>,
    f: F,
) -> Result<()> {
    let mut event_builder = XmlEvent::start_element(element);

    for &(k, v) in attr.iter() {
        event_builder = event_builder.attr(k, v);
    }

    let mut event: XmlEvent<'_> = event_builder.into();
    w.write(event)?;
    f(w)?;
    event = XmlEvent::end_element().into();
    w.write(event).map_err(Into::into)
}

impl From<xml::writer::Error> for Error {
    fn from(error: xml::writer::Error) -> Error {
        Error::XmlError(error)
    }
}

#[cfg(test)]
mod tests {
    use crate::{fields, MarcXml, Record, RecordBuilder};
    use xml::writer::XmlEvent;
    use xml::EmitterConfig;

    fn test_record() -> Record<'static> {
        let mut builder = RecordBuilder::new();
        builder
            .add_fields(fields!(
                data fields: [
                    b"264", b" 1", [
                        b'a' => "León, Spain",
                    ],
                    b"245", b"00", [
                        b'a' => "Book title",
                        b'b' => "Book Subtitle",
                    ],
                    b"100", b"1 ", [
                        b'a' => "Author Name",
                    ],
                    b"041", b"0 ", [
                        b'a' => "eng",
                    ],
                ];
                control fields: [
                    b"008" => "210128t20212021enka    sb    000 0 eng d",
                    b"001" => "000000001",
                ];
            ))
            .unwrap();
        builder.get_record().unwrap()
    }

    fn test_records() -> Vec<Record<'static>> {
        vec![test_record()]
    }

    #[test]
    fn should_output_minified_xml_record() {
        let minified_xml = test_record()
            .xml_minified()
            .map(String::from_utf8)
            .unwrap()
            .unwrap();

        let expected = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\
                               <marc:record xmlns:marc=\"http://www.loc.gov/MARC21/slim\" \
                               xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" \
                               xsi:schemaLocation=\"http://www.loc.gov/MARC21/slim http://www.loc.gov/standards/marcxml/schema/MARC21slim.xsd\">\
                               <marc:leader>00220nam  2200097 i 4500</marc:leader>\
                               <marc:controlfield tag=\"001\">000000001</marc:controlfield>\
                               <marc:controlfield tag=\"008\">210128t20212021enka    sb    000 0 eng d</marc:controlfield>\
                               <marc:datafield tag=\"041\" ind1=\"0\" ind2=\"0\">\
                               <marc:subfield code=\"a\">eng</marc:subfield>\
                               </marc:datafield>\
                               <marc:datafield tag=\"100\" ind1=\"1\" ind2=\"1\">\
                               <marc:subfield code=\"a\">Author Name</marc:subfield>\
                               </marc:datafield>\
                               <marc:datafield tag=\"245\" ind1=\"0\" ind2=\"0\">\
                               <marc:subfield code=\"a\">Book title</marc:subfield>\
                               <marc:subfield code=\"b\">Book Subtitle</marc:subfield>\
                               </marc:datafield>\
                               <marc:datafield tag=\"264\" ind1=\" \" ind2=\" \">\
                               <marc:subfield code=\"a\">León, Spain</marc:subfield>\
                               </marc:datafield>\
                               </marc:record>".to_string();
        assert_eq!(minified_xml, expected);
    }

    #[test]
    fn should_output_pretty_xml_record() {
        let minified_xml = test_record()
            .xml_pretty()
            .map(String::from_utf8)
            .unwrap()
            .unwrap();

        let expected = "\
            <?xml version=\"1.0\" encoding=\"utf-8\"?>\n\
            <marc:record \
                xmlns:marc=\"http://www.loc.gov/MARC21/slim\" \
                xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" \
                xsi:schemaLocation=\"http://www.loc.gov/MARC21/slim http://www.loc.gov/standards/marcxml/schema/MARC21slim.xsd\"\
            >\n  \
                <marc:leader>00220nam  2200097 i 4500</marc:leader>\n  \
                <marc:controlfield tag=\"001\">000000001</marc:controlfield>\n  \
                <marc:controlfield tag=\"008\">210128t20212021enka    sb    000 0 eng d</marc:controlfield>\n  \
                <marc:datafield tag=\"041\" ind1=\"0\" ind2=\"0\">\n    \
                    <marc:subfield code=\"a\">eng</marc:subfield>\n  \
                </marc:datafield>\n  <marc:datafield tag=\"100\" ind1=\"1\" ind2=\"1\">\n    \
                    <marc:subfield code=\"a\">Author Name</marc:subfield>\n  \
                </marc:datafield>\n  \
                <marc:datafield tag=\"245\" ind1=\"0\" ind2=\"0\">\n    \
                    <marc:subfield code=\"a\">Book title</marc:subfield>\n    \
                    <marc:subfield code=\"b\">Book Subtitle</marc:subfield>\n  \
                </marc:datafield>\n  \
                <marc:datafield tag=\"264\" ind1=\" \" ind2=\" \">\n    \
                    <marc:subfield code=\"a\">León, Spain</marc:subfield>\n  \
                </marc:datafield>\n\
            </marc:record>".to_string();

        assert_eq!(minified_xml, expected);
    }

    #[test]
    fn should_output_minified_xml_collection() {
        let minified_xml = test_records()
            .xml_minified()
            .map(String::from_utf8)
            .unwrap()
            .unwrap();

        let expected = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\
                               <marc:collection xmlns:marc=\"http://www.loc.gov/MARC21/slim\" \
                               xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" \
                               xsi:schemaLocation=\"http://www.loc.gov/MARC21/slim http://www.loc.gov/standards/marcxml/schema/MARC21slim.xsd\">\
                               <marc:record>\
                               <marc:leader>00220nam  2200097 i 4500</marc:leader>\
                               <marc:controlfield tag=\"001\">000000001</marc:controlfield>\
                               <marc:controlfield tag=\"008\">210128t20212021enka    sb    000 0 eng d</marc:controlfield>\
                               <marc:datafield tag=\"041\" ind1=\"0\" ind2=\"0\">\
                               <marc:subfield code=\"a\">eng</marc:subfield>\
                               </marc:datafield>\
                               <marc:datafield tag=\"100\" ind1=\"1\" ind2=\"1\">\
                               <marc:subfield code=\"a\">Author Name</marc:subfield>\
                               </marc:datafield>\
                               <marc:datafield tag=\"245\" ind1=\"0\" ind2=\"0\">\
                               <marc:subfield code=\"a\">Book title</marc:subfield>\
                               <marc:subfield code=\"b\">Book Subtitle</marc:subfield>\
                               </marc:datafield>\
                               <marc:datafield tag=\"264\" ind1=\" \" ind2=\" \">\
                               <marc:subfield code=\"a\">León, Spain</marc:subfield>\
                               </marc:datafield>\
                               </marc:record>\
                               </marc:collection>".to_string();
        assert_eq!(minified_xml, expected);
    }

    #[test]
    fn should_output_pretty_xml_collection() {
        let minified_xml = test_records()
            .xml_pretty()
            .map(String::from_utf8)
            .unwrap()
            .unwrap();

        let expected = "\
            <?xml version=\"1.0\" encoding=\"utf-8\"?>\n\
            <marc:collection \
                xmlns:marc=\"http://www.loc.gov/MARC21/slim\" \
                xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" \
                xsi:schemaLocation=\"http://www.loc.gov/MARC21/slim http://www.loc.gov/standards/marcxml/schema/MARC21slim.xsd\"\
            >\n  \
                <marc:record>\n    \
                    <marc:leader>00220nam  2200097 i 4500</marc:leader>\n    \
                    <marc:controlfield tag=\"001\">000000001</marc:controlfield>\n    \
                    <marc:controlfield tag=\"008\">210128t20212021enka    sb    000 0 eng d</marc:controlfield>\n    \
                    <marc:datafield tag=\"041\" ind1=\"0\" ind2=\"0\">\n      \
                        <marc:subfield code=\"a\">eng</marc:subfield>\n    \
                    </marc:datafield>\n    \
                    <marc:datafield tag=\"100\" ind1=\"1\" ind2=\"1\">\n      \
                        <marc:subfield code=\"a\">Author Name</marc:subfield>\n    \
                    </marc:datafield>\n    \
                    <marc:datafield tag=\"245\" ind1=\"0\" ind2=\"0\">\n      \
                        <marc:subfield code=\"a\">Book title</marc:subfield>\n      \
                        <marc:subfield code=\"b\">Book Subtitle</marc:subfield>\n    </marc:datafield>\n    \
                    <marc:datafield tag=\"264\" ind1=\" \" ind2=\" \">\n      \
                        <marc:subfield code=\"a\">León, Spain</marc:subfield>\n    \
                    </marc:datafield>\n  \
                </marc:record>\n\
            </marc:collection>".to_string();

        assert_eq!(minified_xml, expected);
    }

    #[test]
    fn should_write_element() {
        let mut buffer = Vec::new();
        let mut writer = EmitterConfig::new()
            .write_document_declaration(false)
            .create_writer(&mut buffer);

        super::write_element(
            "test_element",
            vec![("attr1", "value1"), ("attr2", "value2")],
            &mut writer,
            |w| {
                // Write some content to the element
                let event = XmlEvent::characters("test content");
                w.write(event).map_err(Into::into)
            },
        )
        .ok();
        let xml_str = String::from_utf8(buffer).unwrap();

        let expected_xml =
            r#"<test_element attr1="value1" attr2="value2">test content</test_element>"#;
        assert_eq!(xml_str, expected_xml);
    }
}
