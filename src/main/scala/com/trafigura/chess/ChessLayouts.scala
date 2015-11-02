package com.trafigura.chess

class ChessLayouts(trace: Boolean, m: Int, n: Int, pieces: Piece*) {
  /**
   * Determines whether the specified piece is not under theratening.
   * @param p piece to be placed
   * @param coord coordinate to be placed on
   * @param pp specifies previously placed piece with its coordinates
   * @return true if the piece can be placed
   */
  protected def nonThreatening(p: Piece, coord: (Int, Int))(pp: ((Int, Int), Piece)) =
    !p(coord, pp._1) && !pp._2(pp._1, coord)

  /**
   * Searches for all available layouts.
   * @return layouts
   */
  def findLayouts: Set[Layout] = {
    var count = 0L

    /**
     * Places remaining pieces on the deck.
     * @param remaining remaining pieces to be placed
     * @param placed already placed pieces
     * @param deck remaining places on the deck
     * @param acc collected layouts
     * @return layouts
     */
    def placePieces(remaining: List[Piece], placed: Layout, deck: Deck, acc: Set[Layout]): Set[Layout] = {
      traceInfo("Placing %s on %s already placed %s", remaining, deck, placed)
      remaining match {
        case Nil =>
          traceInfo("Found layout %s", placed)
          acc + placed
        case piece :: tail =>
          val free = deck.filter(c => placed.forall(nonThreatening(piece, c)))
          if (free.isEmpty) acc
          else
            free.flatMap {c => placePieces(tail, placed + (c -> piece), free - c, acc)}
      }
    }

    val emptyDeck = (for (i <- 1 to m; j <- 1 to n) yield (i, j)).toSet
    placePieces(pieces.toList, Map.empty, emptyDeck, Set.empty)
  }

  def printLayouts: Unit = {
    findLayouts foreach printLayout(m, n)
  }

  def traceInfo(msg: String, params: Any*) = if (trace) println(msg.format(params :_*))
}