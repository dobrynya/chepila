package com.trafigura.chess

object PrintLayoutsByAnotherVariant extends App {
  val chessVariants = new ChessVariants(3, 3, King, King, Roak)
  chessVariants.printLayouts
}

object TestPieces extends App {
  assert(King((2, 2), (1, 1)))
  assert(King((2, 2), (2, 1)))
  assert(King((2, 2), (3, 1)))
  assert(King((2, 2), (1, 2)))
  assert(King((2, 2), 1 -> 3))
  assert(King(2 -> 2, 3 -> 2))
  assert(King(2 -> 2, 3 -> 3))
  assert(King(2 -> 2, 2 -> 3))
  assert(!King(2 -> 2, 2 -> 5))
  assert(!King(1 -> 1, 3 -> 1))
  assert(!King(1 -> 1, 1 -> 3))

  assert(Bishop(1 -> 1, 2 -> 2))
  assert(Bishop(1 -> 1, 3 -> 3))
  assert(!Bishop(1 -> 1, 2 -> 1))
  assert(!Bishop(1 -> 1, 3 -> 1))
  assert(!Bishop(1 -> 1, 2 -> 1))
  assert(!Bishop(1 -> 1, 1 -> 2))
  assert(!Bishop(1 -> 1, 1 -> 3))
  assert(!Bishop(1 -> 1, 3 -> 2))

  assert(Set(PlacedPiece2(King, (1, 2)), PlacedPiece2(King, (3, 1))) == Set(PlacedPiece2(King, (1, 2)), PlacedPiece2(King, (3, 1))))
}