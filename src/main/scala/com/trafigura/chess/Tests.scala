package com.trafigura.chess

object PrintLayoutsForTestCase extends App {
  val pieces = List(King, King, Queen, Bishop, Rook, Knight)
  val chessVariants = new ChessLayouts(false, 9, 6, pieces :_*)
  println(s"Test case is to find all layouts for $pieces on 9x6 deck")
  private val layouts = chessVariants.findLayouts.size
  println(s"Result is $layouts")
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

  assert(Rook(1 -> 1, 2 -> 1))
  assert(Rook(1 -> 1, 2 -> 1))
  assert(Rook(1 -> 1, 1 -> 3))
  assert(Rook(2 -> 2, 2 -> 1))
  assert(Rook(2 -> 2, 2 -> 3))
  assert(Rook(2 -> 2, 3 -> 2))
  assert(!Rook(2 -> 2, 1 -> 1))
  assert(!Rook(2 -> 2, 3 -> 3))

  assert(Knight(1 -> 1, 2 -> 3))
  assert(Knight(1 -> 1, 3 -> 2))
  assert(Knight(1 -> 1, 2 -> 3))
  assert(!Knight(1 -> 1, 2 -> 2))
  assert(!Knight(1 -> 1, 1 -> 3))
  assert(!Knight(1 -> 1, 3 -> 3))

  assert(Queen(1 -> 1, 2 -> 1))
  assert(Queen(1 -> 1, 2 -> 2))
  assert(Queen(1 -> 1, 3 -> 3))
  assert(Queen(1 -> 1, 1 -> 2))
  assert(Queen(1 -> 1, 1 -> 3))
  assert(!Queen(1 -> 1, 2 -> 3))
  assert(!Queen(1 -> 1, 3 -> 2))
}