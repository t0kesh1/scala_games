import scala.io.Source
import scala.util.parsing.combinator._

case class pdParser(filename:String){
    val s = Source.fromFile(filename).toList.mkString
    val lines = raw";\n".r split (raw"(?<!;)\n".r.replaceAllIn(s," ")) toList

    def show() = lines foreach println
}

/*object puredataFileParser extends JavaTokenParsers {
  def json: Parser[JsonValue] = value
  def objects: Parser[Object] = "{" ~> repsep(member, ",") <~ "}" ^^ {case x => Object(x.toMap[Str,JsonValue])}
  def n = "null" ^^ {_  => Str("null")}
  def bool: Parser[Bool] = ("true" | "false") ^^ {case "true" => Bool(true) case "false" => Bool(false)}
  def member: Parser[(Str,JsonValue)] = str ~ (":" ~> (value)) ^^ { case a ~ b => (a,b) }
  def str: Parser[Str] = stringLiteral ^^ {s => Str(s.tail.init)}
  def value: Parser[JsonValue] = floatingPointNumber ^^ {s:String =>Number(s.toFloat)} | str | objects | array | bool | n
  def array: Parser[JArray] = "[" ~> repsep( value ,",") <~ "]" ^^ {JArray(_)}
}*/

case class DirectedGraph[A](nodes:List[A],edges:List[(Int,Int)])