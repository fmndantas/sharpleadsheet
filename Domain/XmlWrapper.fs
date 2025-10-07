module Domain.XmlWrapper

open System.Xml.Linq

let attribute (key: string) (value: string) = XAttribute(XName.Get key, value)

let elementWithAttributes (name: string) (attributes: XAttribute list) (content: XElement list) =
  XElement(XName.Get name, attributes, content)

let element (name: string) (content: XElement list) = elementWithAttributes name [] content

let leafElement (name: string) (content: string) : XElement = XElement(XName.Get name, content)

let document (root: XElement) = XDocument(root)

let normalizeXml (xml: XDocument) : string =
  xml.ToString(SaveOptions.DisableFormatting)

let normalizeXmlText (xmlText: string) : string =
  XDocument.Parse(xmlText) |> normalizeXml
