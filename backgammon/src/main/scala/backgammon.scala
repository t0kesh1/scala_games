import java.io._
import scala.util.Random
import java.util.Scanner

package backgammon
{
case class Board(board:Vector[Int],bar_w:Int,bar_b:Int){
  override def toString: String = {
    def trans(n:Int,i:Int): String = n match{
        case 0 => " "
        case x if(x>=i) => "●"
        case y if(y<=(-i)) => "○"
        case _ => " "
    }
    val s = for(col <- (6 to -6 by -1)) yield{ col match {
        case x if(x>0) => s"${board.slice(12,18).map(trans(_,col)).mkString("|","|","|")}|${board.slice(18,24).map(trans(_,col)).mkString("|","|","|")}  |${trans(bar_w,col)}"
        case 0 => "--------------------------------"
        case x if(x<0) => s"${board.slice(6,12).reverseMap(trans(_,-col)).mkString("|","|","|")}|${board.slice(0,6).reverseMap(trans(_,-col)).mkString("|","|","|")}  |${trans(-bar_b,-col)}"
      }}
    
    s.mkString("131415161718   192021222324\n","\n","\n121110 9 8 7   6 5 4 3 2 1")

  }
  def hit(i:Int): Board = copy(board.updated(i,1),bar_b = bar_b + 1)
  def in = copy(bar_w = bar_w-1)
  def put(i:Int): Board = copy(board.updated(i,board(i)+1))
  def pick(i:Int): Board = copy(board.updated(i,board(i)-1))
  val anchor = board.lastIndexWhere(_ > 0) + 1
  def cantMove(i:Int): Boolean = {
    if(anchor >= 6 && i < 0) return true
    else board(i) <= -2 
  }
  def forthMove(num:Int): List[Board] = if(move(num).isEmpty) List(this) else move(num)
  def move(num:Int): List[Board] = {
    val enter = board(24-num) match {
      case x if(x >= 0) => List(this.in.put(24-num))
      case -1 => List(this.in.hit(24-num))
      case _ => List()
    }
    val upper = for(i <- List.range(num,24);
                    if(board(i)>0);
                    if(board(i-num) > -2)) yield {
                      if(board(i-num)== -1) this.pick(i).hit(i-num) else this.pick(i).put(i-num)
                    }
    val bearoff = if(anchor<=6&&board(num-1)>0) List(this.pick(num-1)) else List() 
    val under = if(anchor>num) List.empty else {
      List(this.pick(anchor-1))
    } 
    if (bar_w > 0) enter else bearoff ::: under ::: upper
    //bearoff ::: enter ::: under ::: upper
  }


  def opposite:Board = Board(board.reverse.map(0 - _),bar_b,bar_w)
  
  }

class backgammon{
  //val board = new Board(new Array[Int](24))
  //val ois = new java.io.ObjectInputStream(new FileInputStream("EVTable"))
  //val EVTable = ois.readObject().asInstanceOf[scala.collection.immutable.Map[List[Int],List[Double]]]
  //ois.close();

  def genMove(b:Board,rolls:(Int,Int)): List[Board] = rolls match{
      case (x,y) if x==y => b.forthMove(x).flatMap(_.forthMove(x)).flatMap(_.forthMove(x)).flatMap(_.forthMove(x)).distinct
      case (x,y) => {val asdf = (b.move(x).flatMap(_.forthMove(y)) ::: (b.move(y).flatMap(_.forthMove(x)))).distinct; if(asdf.isEmpty) List(b) else asdf}
  }
    
  
  def readInt: Int = {
    def safeStringToInt(str: String): Option[Int] = {
      import scala.util.control.Exception._
      catching(classOf[NumberFormatException]) opt str.toInt
    }
    val sc = new Scanner(System.in)
    val num = safeStringToInt(sc.next())
    num match{
      case Some(x) => x
      case None => readInt
    }
  }
  def play = {
    val v = new Random
    val sc = new java.util.Scanner(System.in)
    def firstRoll:(Int,Int) = (v.nextInt(6)+1,v.nextInt(6)+1) match {case (x,y) if(x!=y) => (x,y);case _ => firstRoll}
    val init = Board(Vector(-2,0,0,0,0,5, 0,3,0,0,0,-5, 5,0,0,0,-3,-0,-5,0,0,0,0,2),0,0)
    def turn(board:Board,Roll:(Int,Int),retry:Boolean):Unit = {
      val nextMove = genMove(board,Roll)
      for(i <- nextMove.indices){println(s"${nextMove(i)},${i}\n")}
      println(Roll)
      if (retry) println("out of range..")
      readInt match {
        case -1 => return
        case x if nextMove.isDefinedAt(x) => turn(nextMove(x).opposite, (v.nextInt(6)+1,v.nextInt(6)+1),false)
        case x if !nextMove.isDefinedAt(x) => turn(board,Roll,true)
      }
    }
    turn(init,firstRoll,false)

  }

  def play(b:Board) = {
    val v = new Random
    val sc = new java.util.Scanner(System.in)
    def firstRoll:(Int,Int) = (v.nextInt(6)+1,v.nextInt(6)+1) match {case (x,y) if(x!=y) => (x,y);case _ => firstRoll}
    val init = Board(Vector(-2,0,0,0,0,5, 0,3,0,0,0,-5, 5,0,0,0,-3,-0,-5,0,0,0,0,2),0,0)
    def turn(board:Board,Roll:(Int,Int),retry:Boolean):Unit = {
      val nextMove = genMove(board,Roll)
      for(i <- nextMove.indices){println(s"${nextMove(i)},${i}\n")}
      println(Roll)
      if (retry) println("out of range..")
      readInt match {
        case -1 => return
        case x if nextMove.isDefinedAt(x) => turn(nextMove(x).opposite, (v.nextInt(6)+1,v.nextInt(6)+1),false)
        case x if !nextMove.isDefinedAt(x) => turn(board,Roll,true)
      }
    }
    turn(b,(v.nextInt(6)+1,v.nextInt(6)+1),false)

  }

}



/* バグ　1ポイント目のヒットができない 済
131415161718   192021222324
| | | | | | ||| | | | | | |  |
| | | | | | ||| | | | | | |  |
| | | | | | ||| | | | | | |  |
| | | | | | |||○|○|○| | | |  |
| | | | | | |||○|○|○|○|○|○|  |
| | | |●| |●|||○|○|○|○|○|○|  |●
--------------------------------
| | | | |●|●|||●|●|●| | | |  |
| | | | |●|●|||●|●|●| | | |  |
| | | | |●|●||| | | | | | |  |
| | | | | | ||| | | | | | |  |
| | | | | | ||| | | | | | |  |
| | | | | | ||| | | | | | |  |
121110 9 8 7   6 5 4 3 2 1,0]

todo 添え字周りの再確認
*/

}