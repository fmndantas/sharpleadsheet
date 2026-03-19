module Domain.XmlWrapper

open System.Xml.Linq

[<AutoOpen>]
module private Helpers =
  let removeSelfClosingSpaces (xml: string) : string =
    System.Text.RegularExpressions.Regex.Replace(xml, @" />", "/>")

let attribute (key: string) (value: string) : XAttribute = XAttribute(XName.Get key, value)

let elementWithAttributes (name: string) (attributes: XAttribute list) (content: XElement list) : XElement =
  XElement(XName.Get name, attributes, content)

let element (name: string) (content: XElement list) : XElement = elementWithAttributes name [] content

let leafElement (name: string) (content: string) : XElement = XElement(XName.Get name, content)

let selfEnclosingElementWithAttributes (name: string) (attributes: XAttribute list) : XElement =
  elementWithAttributes name attributes []

let selfEnclosingElement (name: string) : XElement =
  selfEnclosingElementWithAttributes name []

let document (root: XElement) : XDocument = XDocument root

let minifyPlainText (xml: string) : string =
  let doc = XDocument.Parse xml

  doc.ToString SaveOptions.DisableFormatting |> removeSelfClosingSpaces

let formatXDocument (xDocument: XDocument) =
  xDocument |> toString |> minifyPlainText

let formatXElement (xElement: XElement) = xElement |> toString |> minifyPlainText
