package MJ

import scala.math.Ordering
import scala.language.postfixOps

sealed class Kind
case object アタマ extends Kind
case object 明順子 extends Kind
case object 暗順子 extends Kind
case object 明刻子 extends Kind
case object 暗刻子 extends Kind
case object 明槓子 extends Kind
case object 暗槓子 extends Kind

case class ブロック(kind:Kind,hai:Pai){
  def toList: List[Pai] = kind match {
    case アタマ => List(hai,hai)
    case 明順子 => List(hai,hai.next,hai.next.next)
    case 暗順子 => List(hai,hai.next,hai.next.next)
    case 明刻子 => List(hai,hai,hai)
    case 暗刻子 => List(hai,hai,hai)
    case 明槓子 => List(hai,hai,hai,hai)
    case 暗槓子 => List(hai,hai,hai,hai)
    case _ => Nil
  }
  def ID = toList.map(Pai.id).foldLeft(1)(_ * _)

}

object 対子{
  def unapply(b:ブロック): Option[Pai] = b.kind match{
    case アタマ => Some(b.hai)
    case _ => None
  } 
}
object 刻子{
  def unapply(b:ブロック): Option[Pai] = b.kind match{
    case 明刻子 | 暗刻子 | 明槓子 | 暗槓子 => Some(b.hai)
    case _ => None
  } 
}
object 槓子{
  def unapply(b:ブロック): Option[Pai] = b.kind match{
    case 明槓子 | 暗槓子 => Some(b.hai)
    case _ => None
  } 
}

object 順子{
  def unapply(b:ブロック): Option[Pai] = b.kind match{
    case 明順子 | 暗順子  => Some(b.hai)
    case _ => None
  } 
}