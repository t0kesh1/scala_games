package MJ

object Pai{
  val id:Map[Pai,Int] = List(M(1), P(1), S(1), Z(1), M(2), P(2), S(2), Z(2), M(3), P(3), S(3), Z(3), M(4), P(4), S(4), Z(4), M(5), P(5), S(5), Z(5), M(6), P(6), S(6), Z(6), M(7), P(7), S(7), Z(7), M(8), P(8), S(8), M(9), P(9), S(9))
    .zip(List(2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139))
	  .toMap
}

sealed trait Pai{
  def next: Pai
}
case class M(i:Int) extends Pai{ def next: M = M(i + 1)}
case class P(i:Int) extends Pai{ def next: P = P(i + 1)}
case class S(i:Int) extends Pai{ def next: S = S(i + 1)}
case class Z(i:Int) extends Pai{ def next: Z = Z(i)}
