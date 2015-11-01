package com.trafigura.chess

object PrintLayoutsSize extends App {

  val pieces = List(King, King, Queen, Bishop, Roak, Knight)
  val chessLayouts = new ChessLayout(9, 6, pieces :_*)

  println("Found %s possible layouts for deck 9x6 of %s".format(
    chessLayouts.findLayouts.size, pieces))
}


object PrintLayouts extends App {
  new ChessLayout(9, 6, King, King, Queen, Bishop, Roak, Knight).printLayouts
}