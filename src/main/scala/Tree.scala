sealed abstract class Tree[+T]{
  def fold[A](acc:A)(op:(T,T,A)=>A)
}
case class Node[T](value:T,left:Tree[T],right:Tree[T]) extends Tree[T]
case object Nil extends Tree[Nothing]

