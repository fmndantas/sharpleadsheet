module Domain.XmlWrapper

open System.Xml.Linq

let attribute (key: string) (value: string) : XAttribute = XAttribute(XName.Get key, value)

let elementWithAttributes (name: string) (attributes: XAttribute list) (content: XElement list) : XElement =
  XElement(XName.Get name, attributes, content)

let element (name: string) (content: XElement list) : XElement = elementWithAttributes name [] content

let leafElement (name: string) (content: string) : XElement = XElement(XName.Get name, content)

let selfEnclosingElement (name: string) : XElement = XElement(XName.Get name, null)

let document (root: XElement) : XDocument = XDocument root

let minifyXDocument (v: XDocument) : string =
  v.ToString SaveOptions.DisableFormatting

let minifyXElement (v: XElement) : string =
  v.ToString SaveOptions.DisableFormatting

let minifyXmlText (v: string) : string = XDocument.Parse v |> minifyXDocument
