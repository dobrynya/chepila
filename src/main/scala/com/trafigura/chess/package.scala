package com.trafigura

import math._

package object chess {
  type Deck = List[(Int, Int)]
  type Layout = Map[(Int, Int), Piece]
  type Piece = ((Int, Int), (Int, Int)) => Boolean

  case object Bishop extends Piece {
    def apply(c1: (Int, Int), c2: (Int, Int)) = abs(c1._1 - c2._1) == abs(c1._2 - c2._2)

    override def toString() = "Bishop"
  }

  case object Roak extends Piece {
    def apply(c1: (Int, Int), c2: (Int, Int)) = c1._1 == c2._1 || c1._2 == c2._2

    override def toString() = "Roak"
  }

  case object Queen extends Piece {
    def apply(c1: (Int, Int), c2: (Int, Int)) = Bishop(c1, c2) || Roak(c1, c2)

    override def toString() = "Queen"
  }

  case object King extends Piece {
    def apply(c1: (Int, Int), c2: (Int, Int)) = abs(c1._1 - c2._1) <= 1 && abs(c1._2 - c2._2) <= 1

    override def toString() = "King"
  }

  case object Knight extends Piece {
    def apply(c1: (Int, Int), c2: (Int, Int)) = {
      val dx = abs(c1._1 - c2._1)
      val dy = abs(c1._2 - c2._2)
      (dx == 3 && dy == 1) || (dx == 1 && dy == 3)
    }

    override def toString() = "Knight"
  }

  def printLayout(m: Int, n: Int)(layout: Layout) = {
    println("============================================================================")
    for (j <- 1 to n) {
      for (i <- 1 to m)
      print("%s ".format(layout.getOrElse((i, j), "**").toString.substring(0, 2)))
      println
    }
    println("============================================================================")
  }
}
