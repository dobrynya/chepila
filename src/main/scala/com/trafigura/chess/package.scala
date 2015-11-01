package com.trafigura

import math._

package object chess {

  case class PlacedPiece(p: Piece, i: Int, j: Int)

  case class PlacedPiece2(p: Piece2, c: (Int, Int))

  type Piece = (Int, Int, Int, Int) => Boolean

  type Piece2 = ((Int, Int), (Int, Int)) => Boolean

  case object Bishop extends Piece2 {
    def apply(c1: (Int, Int), c2: (Int, Int)) = abs(c1._1 - c2._1) == abs(c1._2 - c2._2)

    override def toString() = "Bishop"
  }

  case object Roak extends Piece2 {
    def apply(i1: Int, j1: Int, i2: Int, j2: Int) = i1 == i2 || j1 == j2

    def apply(c1: (Int, Int), c2: (Int, Int)) = c1._1 == c2._1 || c1._2 == c2._2

    override def toString() = "Roak"
  }

  case object Queen extends Piece2 {
    def apply(c1: (Int, Int), c2: (Int, Int)) = Bishop(c1, c2) || Roak(c1, c2)

    override def toString() = "Queen"
  }

  case object King extends Piece2 {
    def apply(c1: (Int, Int), c2: (Int, Int)) = abs(c1._1 - c2._1) <= 1 && abs(c1._2 - c2._2) <= 1

    override def toString() = "King"
  }

  case object Knight extends Piece {
    def apply(i1: Int, j1: Int, i2: Int, j2: Int) = {
      val dx = abs(i1 - i2)
      val dy = abs(j1 - j2)
      (dx == 3 && dy == 1) || (dx == 1 && dy == 3)
    }

    override def toString() = "Knight"
  }
}
