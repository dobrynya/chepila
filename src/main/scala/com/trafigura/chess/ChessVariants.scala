package com.trafigura.chess

class ChessVariants(m: Int, n: Int, pieces: Piece2*) {
  type Deck = List[(Int, Int)]

  /**
   * Determines whether the specified piece is not under theratening.
   * @param p piece to be placed
   * @param coord coordinate to be placed on
   * @param pp specifies previously placed piece
   * @return true if the piece can be placed
   */
  protected def nonThreatening(p: Piece2, coord: (Int, Int))(pp: PlacedPiece2) = !pp.p(pp.c, coord) && !p(coord, pp.c)

  def findLayouts: List[Set[PlacedPiece2]] = {

    /**
     * Places remaining pieces on the deck.
     * @param remaining remaining pieces to be placed
     * @param placed already placed pieces
     * @param deck remaining places on the deck
     * @param acc collected layouts
     * @return layouts
     */
    def placePieces(remaining: List[Piece2], placed: Set[PlacedPiece2], deck: Deck, acc: List[Set[PlacedPiece2]]):
      List[Set[PlacedPiece2]] = {
      println("Placing " + remaining + " on " + deck + " already placed " + placed)

      remaining match {
        case Nil =>
          println("Found layout " + placed)
          if (acc.nonEmpty && acc.forall(_ == placed)) {
            println("Already contained in layouts, will not be added")
            acc
          } else
            placed :: acc
        case piece :: tail =>
          val free = deck.filter(c => placed.forall(nonThreatening(piece, c)))
          if (free.isEmpty) {
            println("There is no place for " + piece)
            acc
          }
          else {
            println("Available places for " + piece + " are " + free)
            free.flatMap {c =>
              placePieces(tail,
                placed + PlacedPiece2(piece, c),
                deck.filterNot(c == _),
                acc)
            }
          }
      }
    }

    val emptyDeck = (for (i <- 1 to m; j <- 1 to n) yield (i, j)).toList

    pieces.permutations.flatMap(permutation => placePieces(pieces.toList, Set.empty, emptyDeck, Nil)).toList
  }

  private[this] def printLayout(placed: Set[PlacedPiece2]) = {
    val byCoordinates = placed.map(pp => pp.c -> pp.p).toMap
    for (j <- 1 to n) {
      for (i <- 1 to m)
        print("%s ".format(byCoordinates.getOrElse((i, j), "**").toString.substring(0, 2)))
      println
    }
    println("\n============================================================================\n")
  }

  def printLayouts = {
    findLayouts foreach printLayout
  }
}
