import scala.annotation.tailrec

object Chess{

  def sym(ls:List[Pos]): List[Pos] = {
    val l = ls match {
      case Nil => Nil
      case Pos(f, r) :: _ if(f > 3 && r > 3) => ls.map(p => Pos(7-p.f,7-p.r))
      case Pos(f, _) :: _ if(f > 3) => ls.map(p => Pos(7-p.f,p.r))
      case Pos(_, r) :: _ if(r > 3) => ls.map(p => Pos(p.f,7-p.r))
      case _ => ls
    }
    l.find(p=>p.f !=p.r) match{
      case None => l
      case Some(Pos(f,r)) if(f < r) => l
      case Some(Pos(f,r)) if(f > r) => l.map(p => Pos(p.r,p.f))
    }
  }

  def symboard(b:Board): Board = {
    val (poses,pieces) = b.board.toList.sortBy(p=>p._1.f*8+p._1.r).sortBy(_._2 match{
      case K(false) => 0
      case Q(false) => 1
      case R(false) => 2
      case B(false) => 3
      case N(false) => 4
      case P(false) => 5
      case K(true) => 6
      case Q(true) => 7
      case R(true) => 8
      case B(true) => 9
      case N(true) => 10
      case P(true) => 11
    }).unzip
    Board(sym(poses).zip(pieces).toMap,b.Turn)}


  def GenerateBoard(pieces:List[Piece]): List[(Int,Int,Boolean)] = {
    val N = pieces.length
    @tailrec
    def keyList(n:Int,i:Int,j:Int,m:Map[Pos,Piece]): Map[Pos,Piece] = n match{
      case 0 => m;
      case n if(n>0) => keyList(n-1,i>>3,j>>3,m+(Pos(i%8,j%8)->pieces(n-1))) }
    
    for(i <- 0 until (1<<(3*N-1));
        j <- 0 until i;
        turn <- Vector(true,false);
        m = keyList(N,i,j,Map[Pos,Piece]());
        if(m.size==N)) yield (i,j,turn)
  }


}
/*&&Board(m,turn).isValid
def unfold[A,B](p:A=>Boolean,h:A=>B,t:A=>A,a:A,acc:List[B]): List[B] = {
  if(p(a)) acc
  else unfold(p,h,t,t(a), h(a)::acc )
}

def loop(i:Int): List[(Int,Int,Boolean)] = {
  def loop2(j:Int): List[(Int,Int,Boolean)] = {}
}
*/
class Chess{
  def hasWinningMove(results:Set[Option[Result]]): Boolean = 
    results.exists(_ match {case Some(Lose(_)) => true
                            case _             => false })

  def isAllLose(results:Set[Option[Result]]): Boolean = 
    results.exists(_ match {case Some(Win(_)) => false
                            case _ => true })
  
  
  def TableBase(allPos:Set[Board],subTable:Map[Board,Result]=Map()): Map[Board,Result] = {
    val Move:Map[Board,Set[Board]] = allPos.map(b=>b->b.NextMove.map(Chess.symboard)).toMap
    val CheckMate:Map[Board,Result] = allPos.collect({
      case check if(check.Mate) => check -> Lose(0)}).toMap
    @tailrec
    def loop(table:Map[Board,Result],i:Int): Map[Board,Result] = {
      val unKnown = allPos.filterNot(table.contains)
      val winBoard = unKnown.collect({case b if(hasWinningMove(Move(b).map(table.get))) => b->Win(i)}).toMap
      val loseBoard = unKnown.collect({case b if(isAllLose(Move(b).map(table.get))) => b->Lose(i)}).toMap

      if(winBoard.isEmpty&&loseBoard.isEmpty) table
      else loop( table++winBoard++loseBoard, i+1 ) 
    }
    loop(CheckMate++subTable,1)
  }

}