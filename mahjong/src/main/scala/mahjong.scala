package MJ
import scala.util.Random


class Mahjong{
	implicit class myList[A](val ls:List[A]){
		def have(sub:List[A]): Boolean = sub.diff(ls).isEmpty
		def have(sub:A): Boolean = ls.contains(sub)
		def equivalent(sl:List[A]): Boolean = sl.diff(ls).isEmpty && ls.diff(sl).isEmpty
	}

	val BlockSet:Set[ブロック] = (for(i <- Set(1,2,3,4,5,6,7,8,9);
								hai <- Set(M(i),P(i),S(i),Z(i));
								k <- Set(アタマ,暗刻子,暗槓子,暗順子);
								if!( hai==Z(8)||hai==Z(9)||(k==暗順子&&i>7)||(k==暗順子&&hai==Z(i)) ) ) yield ブロック(k,hai))
	
		def hola(手牌: List[Pai]): List[List[ブロック]] = {
    	val tehaiID = 手牌.map(Pai.id).foldLeft(BigInt(1))(_ * _)
    	val blockInShupai = BlockSet filter(b => tehaiID % b.ID == 0) toList
		val 一般形:List[List[ブロック]] = (
			for(bs <- blockInShupai.combinations(5);
				if(tehaiID == bs.foldLeft(BigInt(1))(_ * _.ID)) ) yield bs) toList

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

		val toitoi: List[役] = if( blocks forall (b => !(b.kind==暗順子||b.kind==明順子)) ) List(トイトイ) else Nil

		val junchanta: List[役] = if(blocks forall (yaochu)){
			if (blocks exists jihai) {
				if (blocks forall (b => !(b.kind==明順子||b.kind==暗順子))) List(混老頭) else List(チャンタ)}
			else if (blocks forall (b => !(b.kind==明順子||b.kind==暗順子))) List(清老頭) else List(純チャンタ)} else Nil

		val chinItsu: List[役] = {
			blocks collect[Pai,List[Pai]] ( _ match {
			case ブロック(_,Z(_)) => Z(1)
			case ブロック(_,M(_)) => M(1)
			case ブロック(_,P(_)) => P(1)
			case ブロック(_,S(_)) => S(1)
		}) distinct match {
			case List(M(1)) | List(P(1)) | List(S(1)) => List(チンイツ)
			case List(M(1),Z(1)) | List(P(1),Z(1)) | List(S(1),Z(1)) => List(ホンイツ)
			case _ => Nil
		}}

		
        val sangenyaku: List[役] = {
            val sangen = blocks.collect {
                case 刻子(Z(i)) if (i<=5) => ブロック(暗刻子,Z(i))
                case 対子(Z(i)) if (i<=5) => ブロック(アタマ,Z(i))
            }
            sangen match{
                case daisangen if daisangen have List(ブロック(暗刻子,Z(5)),ブロック(暗刻子,Z(6)),ブロック(暗刻子,Z(7))) => List(大三元)
                case syousangen if syousangen have List(ブロック(暗刻子,Z(5)),ブロック(暗刻子,Z(6)),ブロック(アタマ,Z(7))) => List(小三元,白,発)
                case syousangen if syousangen have List(ブロック(暗刻子,Z(5)),ブロック(アタマ,Z(6)),ブロック(暗刻子,Z(7))) => List(小三元,白,中)
                case syousangen if syousangen have List(ブロック(アタマ,Z(5)),ブロック(暗刻子,Z(6)),ブロック(暗刻子,Z(7))) => List(小三元,中,発)
                case nigen if nigen have List(ブロック(暗刻子,Z(5)),ブロック(暗刻子,Z(6))) => List(白,発)
                case nigen if nigen have List(ブロック(暗刻子,Z(5)),ブロック(暗刻子,Z(7))) => List(白,中)
                case nigen if nigen have List(ブロック(暗刻子,Z(6)),ブロック(暗刻子,Z(7))) => List(中,発)
                case haku if haku have ブロック(暗刻子,Z(5)) => List(白)
                case hatsu if hatsu have ブロック(暗刻子,Z(6)) => List(発)
                case tyun if tyun have ブロック(暗刻子,Z(7)) => List(中)
    			case _ => Nil
            }
        }
		
		val iipeiko: List[役] = if(!Menzen) Nil else{
			blocks match {
				case List(対子(_),順子(a), 順子(b), 順子(c), 順子(d)) if(a==b && c == d) => return List(二盃口)
				case List(対子(_),順子(a), 順子(b), _, _) if(a==b) => List(一盃口)
				case List(対子(_),_, 順子(a), 順子(b), _) if(a==b) => List(一盃口)
				case List(対子(_),_,_,順子(a), 順子(b)) if(a==b) => List(一盃口)
				case _ => Nil }
		}

		val ittsu: List[役] = {
			val syuntsu = blocks flatMap 順子.unapply
			if ((syuntsu have List[Pai](M(1),M(4),M(7)))
				.||(syuntsu have List[Pai](S(1),S(4),S(7)))
				.||(syuntsu have List[Pai](P(1),P(4),P(7)))) List(一通) else Nil
		}

		val sansyoku: List[役] = {
			val syuntsu = blocks flatMap 順子.unapply
			if ((1 to 10) exists (i => syuntsu have List[Pai](M(i),P(i),S(i)) )) List(三色) else Nil
		}

		val sandoko: List[役] = {
			val kotsu = blocks flatMap 刻子.unapply
			if ((1 to 10) exists (i => kotsu have List[Pai](M(i),P(i),S(i)) )) List(三同刻) else Nil
		}
		
		val sankantsu: List[役] = blocks flatMap 槓子.unapply length match {
			case 3 => List(三カンツ)
			case 4 => List(四槓子)
			case _=> Nil
		}
		(iipeiko++ittsu++tanyao++toitoi++junchanta++sankantsu++sansyoku++sandoko++chinItsu++sangenyaku)
	}
	def main(args: Array[String]): Unit = {
		val r = new Random
		val haipai = r.shuffle(for(i <- List.range(1,10);
								hai <- List(M(i),P(i),S(i),Z(i));
								if(hai!=Z(8)&&hai!=Z(9)))yield hai).take(14)
		println(haipai)
		hola(haipai)
	}

}
