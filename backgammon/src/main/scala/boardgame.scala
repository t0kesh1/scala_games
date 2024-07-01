abstract trait boardgame{
  abstract class Board
  def genMove(b:Board): Set[Board]
}