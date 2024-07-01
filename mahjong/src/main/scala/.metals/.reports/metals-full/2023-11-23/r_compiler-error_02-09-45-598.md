file:///C:/Users/monoc/OneDrive/ドキュメント/scala/mahjong/src/main/scala/Mentsu.scala
### java.lang.AssertionError: NoDenotation.owner

occurred in the presentation compiler.

action parameters:
offset: 1895
uri: file:///C:/Users/monoc/OneDrive/ドキュメント/scala/mahjong/src/main/scala/Mentsu.scala
text:
```scala
import scala.math.Ordering
import scala.language.postfixOps


sealed trait Pai{
  def next: Pai
}
case class M(i:Int) extends Pai{ def next: M = M(i + 1)}
case class P(i:Int) extends Pai{ def next: P = P(i + 1)}
case class S(i:Int) extends Pai{ def next: S = S(i + 1)}
case class Z(i:Int) extends Pai{ def next: Z = Z(i)}


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
  val id:Map[Pai,Int] = List(M(1), P(1), S(1), Z(1), M(2), P(2), S(2), Z(2), M(3), P(3), S(3), Z(3), M(4), P(4), S(4), Z(4), M(5), P(5), S(5), Z(5), M(6), P(6), S(6), Z(6), M(7), P(7), S(7), Z(7), M(8), P(8), S(8), M(9), P(9), S(9))
    .zip(List(2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137))
	  .toMap
  def ID = toList.map(id).foldLeft(BigInt(1))(_ * _)

}

abstract sealed trait Block{
  val id:Map[Pai,Int] = List(M(1), P(1), S(1), Z(1), M(2), P(2), S(2), Z(2), M(3), P(3), S(3), Z(3), M(4), P(4), S(4), Z(4), M(5), P(5), S(5), Z(5), M(6), P(6), S(6), Z(6), M(7), P(7), S(7), Z(7), M(8), P(8), S(8), M(9), P(9), S(9))
    .zip(List(2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137))
	  .toMap
  def toList:List[Pai] 
  val ID:BigInt
}

object アタマ{
  def unapply(b:ブロック): Option[Pai] = b.kind match{
    case アタマ()@@ => Some(b.hai)
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
```



#### Error stacktrace:

```
dotty.tools.dotc.core.SymDenotations$NoDenotation$.owner(SymDenotations.scala:2582)
	scala.meta.internal.pc.SignatureHelpProvider$.isValid(SignatureHelpProvider.scala:83)
	scala.meta.internal.pc.SignatureHelpProvider$.notCurrentApply(SignatureHelpProvider.scala:92)
	scala.meta.internal.pc.SignatureHelpProvider$.$anonfun$1(SignatureHelpProvider.scala:48)
	scala.collection.StrictOptimizedLinearSeqOps.loop$3(LinearSeq.scala:280)
	scala.collection.StrictOptimizedLinearSeqOps.dropWhile(LinearSeq.scala:282)
	scala.collection.StrictOptimizedLinearSeqOps.dropWhile$(LinearSeq.scala:278)
	scala.collection.immutable.List.dropWhile(List.scala:79)
	scala.meta.internal.pc.SignatureHelpProvider$.signatureHelp(SignatureHelpProvider.scala:48)
	scala.meta.internal.pc.ScalaPresentationCompiler.signatureHelp$$anonfun$1(ScalaPresentationCompiler.scala:375)
```
#### Short summary: 

java.lang.AssertionError: NoDenotation.owner