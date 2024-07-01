import scala.language.implicitConversions
import scala.annotation.tailrec

object Board{
  def apply(pos:Seq[Pos],piece:Seq[Piece],Turn:Boolean): Board = Board(pos.zip(piece).toMap,Turn)
  
  implicit def T2Pos(t:Tuple2[Int,Int]): Pos = Pos(t._1,t._2)

}


case class Board(board:Map[Pos,Piece],Turn:Boolean){
  def fen(s: String): String = {
    @tailrec
    def loop(str:List[Char],res:String,blank:Int): String = str match {
      case Nil => if (blank==0) res else res + blank.toString
      case ' ' :: tail => loop(tail, res, blank+1)
      case x :: tail if  blank == 0 => loop(tail,res + x, 0)
      case x :: tail if  blank != 0 => loop(tail, res + blank + x, 0)
    }
    loop(s.toList,"",0)
  }


  def OK(p:Pos): Option[Pos] = p.f match {
    case a if(0<=a&&a<=7) => p.r match {case b if(0<=b&&b<=7) => Some(Pos(a,b))
      case _ => None}
    case _ => None
  }
  def Attack(p:Pos): Set[Pos] = {
    def Mytake(stream:Stream[Pos]): Stream[Pos] = stream.tail.takeWhile(p => OK(p)!=None && !board.contains(p)).#::(stream.tail.dropWhile(p => OK(p)!=None && !board.contains(p)).head)

    def Left(p:Pos): Stream[Pos] = p #:: Left(Pos(p.f-1,p.r))
    def Right(p:Pos): Stream[Pos] = (p #:: Right(Pos(p.f+1,p.r)))
    def Up(p:Pos): Stream[Pos] = (p #:: Up(Pos(p.f,p.r+1)))
    def Down(p:Pos): Stream[Pos] = (p #:: Down(Pos(p.f,p.r-1)))
    def rightUp(p:Pos): Stream[Pos] = (p #:: rightUp(Pos(p.f+1,p.r+1)))
    def rightDown(p:Pos): Stream[Pos] = (p #:: rightDown(Pos(p.f+1,p.r-1)))
    def leftUp(p:Pos): Stream[Pos] = p #:: leftUp(Pos(p.f-1,p.r+1))
    def leftDown(p:Pos): Stream[Pos] = p #:: leftDown(Pos(p.f-1,p.r-1))

    def RookAttack(p:Pos): Set[Pos] = Set()++Mytake(Left(p))++Mytake(Right(p))++Mytake(Up(p))++Mytake(Down(p))-p

    def BishopAttack(p:Pos): Set[Pos] = Set()++Mytake(leftUp(p))++Mytake(rightUp(p))++Mytake(leftDown(p))++Mytake(rightDown(p))-p

    def KingAttack(p:Pos): Set[Pos] = Set(Pos(p.f-1,p.r+1), Pos(p.f,p.r+1), Pos(p.f+1,p.r+1),
                                      Pos(p.f-1,p.r),                   Pos(p.f+1,p.r),
                                      Pos(p.f-1,p.r-1), Pos(p.f,p.r-1), Pos(p.f+1,p.r-1))

    def kNightAttack(p:Pos): Set[Pos] = Set(Pos(p.f-2,p.r-1), Pos(p.f+2,p.r-1), Pos(p.f-2,p.r+1),
                                        Pos(p.f+2,p.r+1),                   Pos(p.f+1,p.r-2),
                                        Pos(p.f+1,p.r+2), Pos(p.f-1,p.r-2), Pos(p.f-1,p.r+2))

    def PawnAttack(p:Pos,c:Boolean): Set[Pos] = if(c)Set(Pos(p.f-1,p.r+1),Pos(p.f+1,p.r+1)) else Set(Pos(p.f-1,p.r-1),Pos(p.f+1,p.r-1))
 
    val cnddt = board(p) match{
      case K(_) => KingAttack(p)
      case Q(_) => BishopAttack(p)++RookAttack(p)
      case R(_) => RookAttack(p)
      case B(_) => BishopAttack(p)
      case N(_) => kNightAttack(p)
      case P(turn) => PawnAttack(p,turn)
    }
    cnddt.flatMap(OK(_))
  }
  //def isRange: Boolean = board.keys.flatMap(OK(_)) ???

  lazy val hasKing: Boolean = board.count(_._2 == K(true)) == 1 && board.count(_._2 == K(false)) == 1

  def isChecked(c:Boolean): Boolean = if(hasKing){
    val k = board.find(_._2==K(c)).get._1
    EnemyPieces(c).flatMap(Attack(_)).contains(k)
  }else false

  def isValid:Boolean = hasKing && !isChecked(!Turn)
  
  def AllyPieces(turn:Boolean) = board.filter(_._2.Color == turn).keySet
  def EnemyPieces(turn:Boolean) = AllyPieces(!turn)

  def NextMove: Set[Board] = {
    def Move(p:Pos): Set[Pos] = Attack(p)--AllyPieces(Turn)
    AllyPieces(Turn).flatMap(p => Move(p).map(p2 => Board(board-p+(p2->board(p)),!Turn)) )
    .filter(_.isValid)
  }
  /*
  def Piece2FuncVec(pieces:Vector[Piece]): Function1[Vector[Pos],Boolean] = {
    if(pieces.contains(K(true))&&pieces.contains(K(false))) ((_:Vector[Pos])=> false)
    else {
      val Kw = 0


    }

  }
  val (positions,pieces) = board.toVector.unzip
  val Valid:Boolean = Piece2FuncVec(pieces)(positions)
  */
  lazy val Mate:Boolean = if(hasKing){
    //val OppositeKing:Pos = board.find(_._2==K(c)).get._1
    isChecked(Turn) && NextMove.isEmpty
  }else false
}