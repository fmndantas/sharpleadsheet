module Domain.XmlWrapper

open System.Xml.Linq

let attribute (key: string) (value: string) = XAttribute(XName.Get key, value)

let leafElement (name: string) (content: string) : XElement = XElement(XName.Get name, content)

let element (name: string) (attributes: XAttribute list) (content: XElement list) =
    XElement(XName.Get name, attributes, content)

let document (root: XElement) = XDocument(root)
