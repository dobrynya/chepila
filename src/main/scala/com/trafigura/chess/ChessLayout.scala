package com.trafigura.chess



class ChessLayout(m: Int, n: Int, pieces: Piece*) {
  val deck = for (i <- 1 to m; j <- 1 to n) yield (i, j)

  def findLayouts: List[List[PlacedPiece]] = {
    def findLayout(remaining: List[Piece], placedPieces: List[PlacedPiece],
                   acc: List[List[PlacedPiece]]): List[List[PlacedPiece]] =
      if (remaining.isEmpty) placedPieces :: acc
      else deck.find {
        case (i, j) => placedPieces
          .forall {pp => i != pp.i && j != pp.j && !pp.p(pp.i, pp.j, i, j) && !remaining.head(i, j, pp.i, pp.j) }
      } map {
        case (i, j) => findLayout(remaining.tail, PlacedPiece(remaining.head, i, j) :: placedPieces, acc)
      } getOrElse acc

    pieces.toList.permutations.flatMap(findLayout(_, Nil, Nil)).toList
  }

  private[this] def printLayout(placed: List[PlacedPiece]) = {
    val byCoordinates = placed.map(pp => (pp.i, pp.j) -> pp.p).toMap
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