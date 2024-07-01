import scala.language.implicitConversions

class Automaton{
implicit def e2set[A](e:A):Set[A] =  Set[A](e) 
case class DFA[A,B](States:Set[A],Transition:Function1[A,B=>A],Start:A,Accept:Set[A]){
  def delta[X<:Seq[B]](q0:A,seq:X): A = seq.foldLeft(q0)((S,I) => Transition(S)(I))
  def isAccept[X<:Seq[B]](seq:X): Boolean = delta(Start,seq).exists(Accept(_))
}

case class NFA[A,B](States:Set[A],Transition:Function1[A,B=>Set[A]],Start:A,Accept:Set[A]){
  def delta[X<:Seq[B]](q0:A,seq:X): Set[A] = seq.foldLeft(Set(Start))((S,I) => S.flatMap(Transition(_)(I)))
  def isAccept[X<:Seq[B]](seq:X): Boolean = delta(Start,seq).exists(Accept(_))
}


case class eNFA[A,B](States:Set[A],Transition:Function1[A,Option[B]=>Set[A]],Start:A,Accept:Set[A]){
  val eClosure:Map[A,Set[A]] = States.toList.map({ s => {
      var C = Set(s)
      while(C!=C++C.flatMap(Transition(_)(None))){ C = C++C.flatMap(Transition(_)(None)) }
      (s,C)
    }
  }).toMap

  def delta[X<:Seq[B]](q0:A,seq:X): Set[A] = seq.foldLeft(eClosure(Start))((S,I) => S.flatMap(Transition(_)(Some(I))).flatMap(eClosure(_)))
  def isAccept[X<:Seq[B]](seq:X): Boolean = delta(Start,seq).exists(Accept(_))
}

}
class Match[A]
case class Val[A](v:A) extends Match[A]