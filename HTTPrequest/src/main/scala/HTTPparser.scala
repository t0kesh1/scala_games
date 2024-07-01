/*
eBNF
body ::= "{" Str ":" Value { "," Str ":" Value } "}"
Str ::= String
Value ::= Number | String | Lists
Lists ::= "[" body {"," body} "]"
String
*/
package syzygy

import scala.util.parsing.combinator._

sealed trait JsonValue
case class Object(m:Map[Str,JsonValue]) extends JsonValue{
  override def toString = m.toString
}
case class JArray(a:List[JsonValue]) extends JsonValue{
  override def toString = a.toString
}
case class Number(n:Float) extends JsonValue{
  override def toString = n.toString
}
case class Str(s:String) extends JsonValue{
  override def toString = s.toString
}
case class Bool(b:Boolean) extends JsonValue{
  override def toString = b.toString
}


object JSONparser extends JavaTokenParsers {
  def json: Parser[JsonValue] = value
  def objects: Parser[Object] = "{" ~> repsep(member, ",") <~ "}" ^^ {case x => Object(x.toMap[Str,JsonValue])}
  def n = "null" ^^ {_  => Str("null")}
  def bool: Parser[Bool] = ("true" | "false") ^^ {case "true" => Bool(true) case "false" => Bool(false)}
  def member: Parser[(Str,JsonValue)] = str ~ (":" ~> (value)) ^^ { case a ~ b => (a,b) }
  def str: Parser[Str] = stringLiteral ^^ {s => Str(s.tail.init)}
  def value: Parser[JsonValue] = floatingPointNumber ^^ {s:String =>Number(s.toFloat)} | str | objects | array | bool | n
  def array: Parser[JArray] = "[" ~> repsep( value ,",") <~ "]" ^^ {JArray(_)}
}

object FENparser extends JavaTokenParsers {
  def fen = position ~ ("_" ~> turn) ~ ("_"  ~> castling) ~ ("_"  ~> unpassant) ~ ("_" ~> ply) ~ opt( "_"  ~>  move ) ^^ {
      case pos ~ t ~ c ~ u ~ p ~ m => FEN(pos,t,c,u,p,m) }
  def noExist: Parser[String] = "-"
  def position: Parser[String] = rank ~ repN(7, "/" ~> rank) ^^ { case r ~ Nil => r
                                                                  case r ~ ys => r + ys.fold("")(_ + "/" + _)                                                        
                                                                }

  def rank: Parser[String] = rep1(empty | piece) ^^ { case x => x.fold("")(_ + _)}
  def empty = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8"
  def piece =  "K" | "Q" | "R" | "N" | "B" | "P" | "k" | "q" | "r" | "b" | "n" | "p"
  def turn: Parser[String] = "w" | "b"
  def castling: Parser[Option[String]] = noExist ^^ {_ => None} | (opt("K") ~ opt("Q") ~ opt("k") ~ opt("q")) ^^ {case a ~ b ~ c ~ d => Some("KQkq")}
  def unpassant: Parser[Option[String]] = noExist ^^ {_ => None} |( "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" ) ^^ { Option(_)}
  def ply: Parser[Int] = wholeNumber ^^ {_.toInt}
  def move: Parser[Int] = wholeNumber ^^ {_.toInt}
}

case class FEN(position:String,turn:String,castling:Option[String],unpassant:Option[String],ply:Int,move:Option[Int])

object SVGparser extends JavaTokenParsers {
  class MyRichString(str: String) {
    def ignoreCase: Parser[String] = ("""(?i)\Q""" + str + """\E""").r
  }

  implicit def pimpString(str: String): MyRichString = new MyRichString(str)

  def angle = number <~ opt("deg" | "grad" | "rad")


}