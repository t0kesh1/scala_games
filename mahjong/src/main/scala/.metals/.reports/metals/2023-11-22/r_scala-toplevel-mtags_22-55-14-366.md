id: file:///C:/Users/monoc/OneDrive/ドキュメント/scala/mahjong/src/main/scala/Mentsu.scala:[1237..1243) in Input.VirtualFile("file:///C:/Users/monoc/OneDrive/ドキュメント/scala/mahjong/src/main/scala/Mentsu.scala", "import scala.math.Ordering
import scala.language.postfixOps


sealed trait Pai{
  def next: Pai
}
case class M(i:Int) extends Pai{ def next: M = M(i + 1)}
case class P(i:Int) extends Pai{ def next: P = P(i + 1)}
case class S(i:Int) extends Pai{ def next: S = S(i + 1)}
case class Z(i:Int) extends Pai{ def next: Z = Z(i)}
/*
case class 面子(kind:Kind,hai:Pai){
def toList: List[Pai] = kind match{
  case アタマ => List((hai),(hai))
  case 槓子 => List(hai,hai,hai,hai)
  case 刻子 => List(hai,hai,hai)
  case 順子 => List(hai,(hai._1+1,hai._2),(hai._1+2,hai._2))
}
}

*/

abstract sealed trait Block{
  val id:Map[Pai,Int] = List(M(1), P(1), S(1), Z(1), M(2), P(2), S(2), Z(2), M(3), P(3), S(3), Z(3), M(4), P(4), S(4), Z(4), M(5), P(5), S(5), Z(5), M(6), P(6), S(6), Z(6), M(7), P(7), S(7), Z(7), M(8), P(8), S(8), M(9), P(9), S(9))
    .zip(List(2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137))
	  .toMap
  def toList:List[Pai] 
  val ID:BigInt
}

abstract sealed class 面子(hai:Pai) extends Block
case class アタマ(hai:Pai) extends Block{
  override def toList: List[Pai] = List((hai),(hai))
  override val ID = id(hai)*id(hai)
}

abstract sealed class 

object 面子{
  def unapply(m:面子): Option[Pai] = m match { case 槓子(h) => Some(h)
														case 刻子((h),o) => Some((h))
														case 順子((h),o) => Some((h))
														case _ => None }
														
}

object 刻子{
  def unapply(k:Block): Option[(Pai,Boolean)] = k match{
    case 順子(_,_) => None
    case アタマ(_) => None
    case 刻子(h,o) => Some(h,o)
    case 槓子(h) => Some(h,false)
  } 
}


sealed case class 槓子(hai:Pai) extends 面子(hai){
  override def toList: List[Pai] = List(hai,hai,hai,hai)
  override val ID = id(hai)*id(hai)*id(hai)*id(hai)
}

class 明槓子(hai:Pai) extends 槓子(hai)
class 暗槓子(hai:Pai) extends 槓子(hai)

case class 刻子(hai:Pai,open:Boolean) extends 面子(hai){
  override def toList: List[Pai] = List(hai,hai,hai)
  override val ID = id(hai)*id(hai)*id(hai)
}

case class 順子(hai:Pai,open:Boolean) extends 面子(hai){
  override def toList: List[Pai] = List(hai,hai.next,hai.next.next)
  override val ID = id(hai)*id(hai.next)*id(hai.next.next)
}
")
file:///C:/Users/monoc/OneDrive/ドキュメント/scala/mahjong/src/main/scala/Mentsu.scala
file:///C:/Users/monoc/OneDrive/ドキュメント/scala/mahjong/src/main/scala/Mentsu.scala:40: error: expected identifier; obtained object
object 面子{
^
#### Short summary: 

expected identifier; obtained object