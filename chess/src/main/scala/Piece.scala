object Piece{
  def unapply(p:Piece): Option[Boolean] = p match{
    case K(c) => Some(c)
    case Q(c) => Some(c)
    case R(c) => Some(c)
    case B(c) => Some(c)
    case N(c) => Some(c)
    case P(c) => Some(c)
  }
}
trait Piece{val Color:Boolean}
case class K(Color:Boolean) extends Piece
case class Q(Color:Boolean) extends Piece
case class R(Color:Boolean) extends Piece
case class B(Color:Boolean) extends Piece
case class N(Color:Boolean) extends Piece
case class P(Color:Boolean) extends Piece
