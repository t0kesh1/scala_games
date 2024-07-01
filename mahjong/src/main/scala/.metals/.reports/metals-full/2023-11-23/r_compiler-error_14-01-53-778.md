file:///C:/Users/monoc/OneDrive/ドキュメント/scala/mahjong/src/main/scala/mahjong.scala
### java.lang.AssertionError: assertion failed: NoType

occurred in the presentation compiler.

action parameters:
offset: 2223
uri: file:///C:/Users/monoc/OneDrive/ドキュメント/scala/mahjong/src/main/scala/mahjong.scala
text:
```scala
import scala.util.Random

class mahjong{
  
  val BlockSet:Set[ブロック] = (for(i <- Set(1,2,3,4,5,6,7,8,9);
							hai <- Set(M(i),P(i),S(i),Z(i));
							k <- Set(アタマ,暗刻子,暗槓子,暗順子);
							if( hai!=Z(8)||hai!=Z(9)||(k==暗順子&&i<7) ) ) yield ブロック(k,hai))

  val id:Map[Pai,Int] = List(M(1), P(1), S(1), Z(1), M(2), P(2), S(2), Z(2), M(3), P(3), S(3), Z(3), M(4), P(4), S(4), Z(4), M(5), P(5), S(5), Z(5), M(6), P(6), S(6), Z(6), M(7), P(7), S(7), Z(7), M(8), P(8), S(8), M(9), P(9), S(9))
         .zip(List(2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137))
		 .toMap
		 
  def main: Unit = {
    val r = new Random
	val haipai = r.shuffle(for(i <- List.range(1,10);
							 hai <- List(M(i),P(i),S(i),Z(i));
							 if(hai!=Z(8)&&hai!=Z(9)))yield hai).take(14)
	println(haipai)
	this.hola(haipai)
  }
  
  def hola(手牌: List[Pai]): Set[List[ブロック]] = {
    val tehaiID = 手牌.map(id).foldLeft(BigInt(1))(_ * _)
    val blockInShupai = BlockSet.filter(b => tehaiID % b.ID == 0)
	val 一般形:Set[List[ブロック]] = for(b1 <- blockInShupai;
						b2 <- blockInShupai;
						b3 <- blockInShupai;
						b4 <- blockInShupai;
						b5 <- blockInShupai;
						if(tehaiID == b1.ID * b2.ID * b3.ID * b4.ID * b5.ID) ) yield List(b1,b2,b3,b4,b5)

	一般形
  }
  
  def yaku(blocks: List[ブロック]): List[役] = {

    def Menzen: Boolean = blocks forall (b => !Set[Kind](明順子,明刻子,明槓子).contains(b.kind))
    def yaochu(b:ブロック): Boolean = b match {
		case ブロック(明順子,hai) if (hai==M(7)||hai==P(7)||hai==S(7)) => true
		case ブロック(_,Z(_)) => true
		case ブロック(_,hai) if Set[Pai](M(1),P(1),S(1),M(9),P(9),S(9)).contains(hai) => true
		case _ => false}

	def jihai(b:ブロック) = b match {case ブロック(_,Z(_)) => true; case _ => false}
	val tanyao: List[役] = if(blocks forall(b => !yaochu(b))) List(タンヤオ) else Nil
	val toitoi: List[役] = if( blocks forall (b => b.kind!=暗順子||b.kind!=明順子) ) List(トイトイ) else Nil
	val junchanta: List[役] = if(blocks forall (b => yaochu(b))) List(純チャンタ) else Nil
	val chinItsu: List[役] = if (blocks forall (b => b.hai match {case M(_)=> true; case _ => false})) List(チンイツ) else Nil
	
	val sangenyaku: List[役] = {
		val sangen = blocks.collect{case@@}
		kotsu match{
			case daisangen if kotsu have List[Pai](Z(5),Z(6),Z(7)) => List(大三元)
			case syousangen if kotsu 
		}
	  val haku = if(blocks exists (_.hai==Z(5))) Some(白) else None
	  val hatsu = if(blocks exists (_.hai==Z(6))) Some(発) else None
	  val chun = if(blocks exists (_.hai==Z(7))) Some(中) else None
	  (haku++hatsu++chun).toList
	}
	  
	val iipeiko: List[役] = if(!Menzen) Nil else{
	  blocks match {
	    case List(対子(_),順子(a), 順子(b), 順子(c), 順子(d)) if(a==b && c == d) => return List(二盃口)
		case List(対子(_),順子(a), 順子(b), _, _) if(a==b) => List(一盃口)
		case List(対子(_),_, 順子(a), 順子(b), _) if(a==b) => List(一盃口)
		case List(対子(_),_,_,順子(a), 順子(b)) if(a==b) => List(一盃口)
		case _ => Nil }
	  Nil
	}

	val ittsu: List[役] = {
		val syuntsu = blocks flatMap (順子.unapply(_))
		if (syuntsu have List[Pai](M(1),M(4),M(7)) ) List(一通) 
		else if (syuntsu have List[Pai](S(1),S(4),S(7)) ) List(一通) 
		else if (syuntsu have List[Pai](P(1),P(4),P(7)) ) List(一通) else Nil
	}

	val sansyoku: List[役] = {
		val syuntsu = blocks flatMap (順子.unapply(_))
		if ((1 to 10) exists (i => syuntsu have List[Pai](M(i),P(i),S(i)) )) List(三色) else Nil
		Nil
	}

	val sandoko: List[役] = {
		val syuntsu = blocks flatMap (刻子.unapply(_))
		if ((1 to 10) exists (i => syuntsu have List[Pai](M(i),P(i),S(i)) )) List(三同刻) else Nil
		Nil
	}
	
	val sankantsu: List[役] = blocks flatMap (槓子.unapply(_)) length match {
		case 3 => List(三カンツ)
		case 4 => List(四槓子)
		case _=> Nil
	}
	(iipeiko++ittsu++tanyao++toitoi++junchanta++sankantsu++sansyoku++sandoko++chinItsu++sangenyaku)
  }

  implicit class myList[A](val ls:List[A]){
    def have(sub:List[A]): Boolean = sub.diff(ls).isEmpty
	def have(sub:A): Boolean = ls.contains(sub)
	def equivalent(sl:List[A]): Boolean = sl.diff(ls).isEmpty && ls.diff(sl).isEmpty
  }


}

```



#### Error stacktrace:

```
scala.runtime.Scala3RunTime$.assertFailed(Scala3RunTime.scala:8)
	dotty.tools.dotc.core.Types$TypeBounds.<init>(Types.scala:5141)
	dotty.tools.dotc.core.Types$AliasingBounds.<init>(Types.scala:5220)
	dotty.tools.dotc.core.Types$TypeAlias.<init>(Types.scala:5242)
	dotty.tools.dotc.core.Types$TypeAlias$.apply(Types.scala:5279)
	dotty.tools.dotc.core.Types$Type.bounds(Types.scala:1732)
	scala.meta.internal.pc.completions.CaseKeywordCompletion$.contribute(MatchCaseCompletions.scala:154)
	scala.meta.internal.pc.completions.Completions.advancedCompletions(Completions.scala:433)
	scala.meta.internal.pc.completions.Completions.completions(Completions.scala:183)
	scala.meta.internal.pc.completions.CompletionProvider.completions(CompletionProvider.scala:86)
	scala.meta.internal.pc.ScalaPresentationCompiler.complete$$anonfun$1(ScalaPresentationCompiler.scala:123)
```
#### Short summary: 

java.lang.AssertionError: assertion failed: NoType