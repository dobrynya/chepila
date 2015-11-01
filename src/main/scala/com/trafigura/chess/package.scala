package com.trafigura

import math._

package object chess {

  case class PlacedPiece(p: Piece, i: Int, j: Int)

  type Piece = (Int, Int, Int, Int) => Boolean

  case object Bishop extends Piece {
    def apply(i1: Int, j1: Int, i2: Int, j2: Int) = abs(i1 - i2) == abs(j1 - j2)

    override def toString() = "Bishop"
  }

  case object Roak extends Piece {
    def apply(i1: Int, j1: Int, i2: Int, j2: Int) = i1 == i2 || j1 == j2

    override def toString() = "Roak"
  }

  case object Queen extends Piece {
    def apply(i1: Int, j1: Int, i2: Int, j2: Int) = Bishop(i1, j1, i2, j2) || Roak(i1, j1, i2, j2)

    override def toString() = "Queen"
  }

  case object King extends Piece {
    def apply(i1: Int, j1: Int, i2: Int, j2: Int) = {
      val offset = abs(i1 - i2) + abs(j1 - j2)
      1 == offset || 2 == offset
    }

    override def toString() = "King"
  }

  case object Knight extends Piece {
    def apply(i1: Int, j1: Int, i2: Int, j2: Int) = abs(i1 - i2) + abs(j1 - j2) == 4

    override def toString() = "Knight"
  }
}
