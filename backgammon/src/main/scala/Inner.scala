case class Inner(i:(Int,Int,Int,Int,Int,Int)){
  val pip = i._1*6 +i._2*5 + i._3*4 + i._4*3 + i._5*2 + i._6*1
  val count = i._1 +i._2 + i._3 + i._4 + i._5 + i._6

  def pt(p:Int): Int = p match {
    case 6 => i._1
    case 5 => i._2
    case 4 => i._3
    case 3 => i._4
    case 2 => i._5
    case 1 => i._6
  }

  val back = i match {  case (a,_,_,_,_,_) if(a>0) => 6
                        case (0,a,_,_,_,_) if(a>0) => 5
                        case (0,0,a,_,_,_) if(a>0) => 4
                        case (0,0,0,a,_,_) if(a>0) => 3
                        case (0,0,0,0,a,_) if(a>0) => 2
                        case (0,0,0,0,0,a) if(a>0) => 1
                        case (0,0,0,0,0,0) => 0
                        }

  def inner(ls:List[Int]): Inner = ls match {
    case a :: b :: c :: d :: e :: f :: Nil => Inner(a,b,c,d,e,f)
		case _ => throw new NoSuchElementException
  }

  def take(p:Int): Inner = 
	  p match {
      case 6 => Inner(i._1-1,i._2,i._3,i._4,i._5,i._6)
      case 5 => Inner(i._1,i._2-1,i._3,i._4,i._5,i._6)
      case 4 => Inner(i._1,i._2,i._3-1,i._4,i._5,i._6)
      case 3 => Inner(i._1,i._2,i._3,i._4-1,i._5,i._6)
      case 2 => Inner(i._1,i._2,i._3,i._4,i._5-1,i._6)
      case 1 => Inner(i._1,i._2,i._3,i._4,i._5,i._6-1)
	  }

  def put(p:Int): Inner = 
	  p match {
      case 6 => Inner(i._1+1,i._2,i._3,i._4,i._5,i._6)
      case 5 => Inner(i._1,i._2+1,i._3,i._4,i._5,i._6)
      case 4 => Inner(i._1,i._2,i._3+1,i._4,i._5,i._6)
      case 3 => Inner(i._1,i._2,i._3,i._4+1,i._5,i._6)
      case 2 => Inner(i._1,i._2,i._3,i._4,i._5+1,i._6)
      case 1 => Inner(i._1,i._2,i._3,i._4,i._5,i._6+1)
      case _ if(p<1) => this
	  }
  
  def move(dice: Int): List[Inner] = if(this == Inner(0,0,0,0,0,0)) List(this) else (1 to 6).toList.flatMap{ p => pt(p) match{
	case 0 => Nil
    case x if(p >= dice || (p < dice && p == back)) => List(take(p).put(p-dice))
    case x if(p < dice && p != back) => Nil }
  }

  def genPosition(dices:(Int,Int)): List[Inner] = dices match{
    case (a,b) if(a == b) => move(a).flatMap(_.move(a)).flatMap(_.move(a)).flatMap(_.move(a)).distinct
    case (a,b) => (move(a).flatMap(_.move(b)) ::: move(b).flatMap(_.move(a))).distinct
  }
}


case class DPD(p:List[Double]){
  def head = p.head
  def tail = DPD(p.tail)
  def ::(that:Double) = DPD(that :: this.p)
  def *(d:Double): DPD = if(this!=DPD(Nil)) DPD( p.map(x => x*d) ) else DPD(Nil)
  def +(that:DPD): DPD = (this,that) match {
    case (DPD(Nil),DPD(Nil)) => DPD(Nil)
    case (DPD(Nil),y@DPD(_)) => y
    case (x@DPD(_),DPD(Nil)) => x
    case (x,y) => (x.head + y.head) :: (x.tail+y.tail) }

  def EV = p.zipWithIndex.foldLeft(0.0)((acc,dpd) => acc + dpd._1 * dpd._2)

  def EQ(that:DPD): Double = (this,that) match {
    case (DPD(Nil),DPD(Nil)) => 0.0
    case (DPD(Nil),DPD(x)) => x.foldLeft(0.0)((a,b)=> a + b)
    case (DPD(x),DPD(Nil)) => 0
    case (DPD(x),DPD(y)) => x.head * y.foldLeft(0.0)((a,b)=>a + b) + this.tail.EQ(DPD(y.tail))
  }

  override def toString: String = "DPD" + p.mkString("(",",",")")
}
