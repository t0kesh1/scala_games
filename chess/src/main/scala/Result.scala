object Result {
  implicit object MyOrdering extends Ordering[Result] {
override def compare(x:Result,y:Result): Int = (x,y) match{
  case (Win(a),Win(b)) => b - a
  case (Win(_),Draw) | (Win(_),Lose(_))=> 1
  case (Draw,Win(_)) => -1
  case (Draw,Draw) => 0
  case (Draw,Lose(_)) => 1
  case (Lose(a),Lose(b)) => a - b
  case (Lose(_),Draw) | (Lose(_),Win(_)) => -1
  case _ => 0 }
}

}
sealed class Result
case class Win(mate:Int) extends Result
case class Lose(mate:Int) extends Result
case object Draw extends Result
