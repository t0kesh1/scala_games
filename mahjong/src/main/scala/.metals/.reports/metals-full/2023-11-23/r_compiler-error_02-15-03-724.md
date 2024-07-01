file:///C:/Users/monoc/OneDrive/ドキュメント/scala/mahjong/src/main/scala/mahjong.scala
### java.lang.AssertionError: NoDenotation.owner

occurred in the presentation compiler.

action parameters:
offset: 3141
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
  
  def hola(手牌: List[Pai]): List[Block] = {
    val tehaiID = 手牌.map(id).foldLeft(BigInt(1))(_ * _)
    val blockInShupai = BlockSet.filter(b => tehaiID % b.ID == 0)
	val 一般形:Set[List[ブロック]] = for(b1 <- blockInShupai;
						b2 <- blockInShupai;
						b3 <- blockInShupai;
						b4 <- blockInShupai;
						b5 <- blockInShupai;
						if(tehaiID == b1.ID * b2.ID * b3.ID * b4.ID * b5.ID) ) yield List(b1,b2,b3,b4,b5)


    def add面子(blockList:Set[List[Block]]): Set[List[Block]] = blockList.flatMap(blocks => {
								  val taken = 手牌.diff(blocks.flatMap(_.toList))
							  
								  手牌.collect{
									case i:Pai if(taken have List(i,i.next,i.next.next) ) => (順子(i) :: blocks).sortBy(_.toString)
									case i:Pai if(taken have List(i,i,i)) => (刻子(i) :: blocks).sortBy(_.toString)
								  }
								})

    var 雀頭候補:Set[List[Block]] = 手牌.collect{ case i:Pai if(手牌 have(List(i,i))) => List(アタマ(i)) }.toSet
	
	val 向聴3 = add面子(雀頭候補)
	val 向聴2 = add面子(向聴3)
	val 向聴1 = add面子(向聴2)
	val 向聴0 = add面子(向聴1)
	
	if(向聴0.size == 1) return 向聴0.head
	//向聴0 maxBy (ls => yaku(ls) map(a => a._2) sum)
	向聴0.head
  }
  
  def yaku(blocks: List[ブロック]): List[役] = {

    def Menzen: Boolean = blocks forall (b => !Set[Kind](明順子,明刻子,明槓子).contains(b.kind))
    def yaochu(b:ブロック): Boolean = b match {
		case ブロック(明順子,hai) if (hai==M(7)||hai==P(7)||hai==S(7)) => true
		case ブロック(_,Z(i)) => true
		case ブロック(_,hai) if Set[Pai](M(1),P(1),S(1),M(9),P(9),S(9)).contains(hai) => true}
	def jihai(b:ブロック) = b match {case ブロック(_,Z(_)) => true}
	val tanyao: List[役] = if(blocks forall(b => !yaochu(b))) List(タンヤオ) else Nil
	val toitoi: List[役] = if( blocks forall (b => b.kind!=暗順子||b.kind!=明順子) ) List(トイトイ) else Nil
	val junchanta: List[役] = if(blocks forall (b => yaochu(b))) List(純チャンタ) else Nil
	val chinItsu: List[役] = if (blocks forall (b => b match {case ブロック(_,M(_))=> true})) List(チンイツ) else Nil
	
	val sangen: List[役] = {
	  val haku = if(blocks exists (_.hai==Z(5))) Some(白) else None
	  val hatsu = if(blocks exists (_.hai==Z(6))) Some(白) else None
	  val chun = if(blocks exists (_.hai==Z(7))) Some(白) else None
	  (haku++hatsu++chun).toList
	}
	  
	val iipeiko: List[役] = if(Menzen){
	  blocks match {
	    case List(アタマ(_)@@,ブロック(順子,a), ブロック(順子,b), ブロック(順子,c), ブロック(順子,d)) if(a==b && c == d) => return List(二盃口)
		case List(アタマ(_),ブロック(順子,a), ブロック(順子,b), _, _) if(a==b) => List(一盃口)
		case List(アタマ(_),_, ブロック(順子,a), ブロック(順子,b), _) if(a==b) => List(一盃口)
		case List(アタマ(_),_,_,ブロック(順子,a), ブロック(順子,b)) if(a==b) => List(一盃口)
		case _ => Nil }
			Nil
	} else Nil

	val ittsu: List[役] = {
	  for(mps <- List(M,P,S);
	      b1 <- List(true,false);
		  b2 <- List(true,false);
		  b3 <- List(true,false)) if(blocks have(List(順子(mps(1)),順子(mps(4)),順子(mps(7))))) return List(一通)
	  Nil
	}

	val sansyoku: List[役] = {
	  for(num <- 1 to 10;
	      b1 <- List(true,false);
		  b2 <- List(true,false);
		  b3 <- List(true,false)) if(blocks have(List(順子(M(num)),順子(P(num)),順子(S(num))))) return List(三色)
	  Nil
	}
	
	val sandoko: List[役] = {
	  for(num <- 1 to 10;
	      b1 <- List(true,false);
		  b2 <- List(true,false);
		  b3 <- List(true,false)) if(blocks have(List(刻子(S(num)),刻子(S(num)),刻子(S(num))))) return List(三同刻)
	  Nil
	}
	
	val sankantsu: List[役] = blocks match {
	  case アタマ(_) :: 槓子(_) :: 槓子(_) :: 槓子(_) :: _ => List(三カンツ)
	  case アタマ(_) :: _ :: 槓子(_) :: 槓子(_) :: 槓子(_) :: _ => List(三カンツ)
	  case _ => Nil }

	(iipeiko++ittsu++tanyao++toitoi++junchanta++sankantsu++sansyoku++sandoko++chinItsu)
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