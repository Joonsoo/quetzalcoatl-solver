package quetsolver

object Stages {
    def stage1_11 = {
        // 1 -> red
        // 2 -> blue
        Board(
            Cells(Set(
                Spot(1, 1, 1),
                Spot(1, 2, None),
                Spot(1, 3, None),
                Spot(1, 4, None),
                Spot(4, 1, 2),
                Spot(4, 2, None),
                Spot(4, 3, None),
                Spot(4, 4, None),
                Spot(2, 3, None),
                Spot(2, 4, None),
                Spot(3, 3, None),
                Spot(3, 4, None))),
            Set(Line.of("A", Seq(
                Spot(1, 1, 2),
                Spot(1, 2, None),
                Spot(1, 3, None),
                Spot(1, 4, None))),
                Line.of("B", Seq(
                    Spot(4, 1, 1),
                    Spot(4, 2, None),
                    Spot(4, 3, None),
                    Spot(4, 4, None)))))
    }
}

object Main {

    def main(args: Array[String]): Unit = {
        val initialBoard = Stages.stage1_11

        initialBoard.printBoard()
        bfs(initialBoard) match {
            case Some((path, finalBoard)) =>
                println("Found a solution")
                path.foldRight(initialBoard) { (i, board) =>
                    board.printBoard()
                    println(i._1.name, i._2)
                    board.lineMoved(i._1, i._2).get
                }
                finalBoard.printBoard()
            case None =>
                println("Failed to find a solution!")
        }
    }

    def bfs(initialBoard: Board): Option[(List[(Line, LineMovement)], Board)] = bfs(List((List(), initialBoard)), scala.collection.mutable.Set(initialBoard))
    def bfs(queue: List[(List[(Line, LineMovement)], Board)], memo: scala.collection.mutable.Set[Board]): Option[(List[(Line, LineMovement)], Board)] = queue match {
        case (path, head) +: rest =>
            if (head.finished) {
                Some((path, head))
            } else {
                var nextQueue: List[(List[(Line, LineMovement)], Board)] = rest
                head.moveables foreach { m =>
                    val move = m._1
                    val board = m._2
                    if (!(memo contains board)) {
                        memo += board
                        nextQueue :+= (move +: path, board)
                    }
                }
                bfs(nextQueue, memo)
            }
        case List() => None
    }
}

case class Cells(cells: Set[Spot]) {
    val byLoc: Map[(Int, Int), Option[Int]] = (cells map { c => (c.x, c.y) -> c.color }).toMap

    val (minX, maxX) = { val xs = (cells map { _.x }); (xs.min, xs.max) }
    val (minY, maxY) = { val ys = (cells map { _.y }); (ys.min, ys.max) }
}
object Board {
    val movements = Seq(
        LineMovement(true, -1, 0),
        LineMovement(true, 1, 0),
        LineMovement(true, 0, -1),
        LineMovement(true, 0, 1),
        LineMovement(false, -1, 0),
        LineMovement(false, 1, 0),
        LineMovement(false, 0, -1),
        LineMovement(false, 0, 1))
}
case class Board(cells: Cells, lines: Set[Line]) {
    val linesByLoc: Map[(Int, Int), Line] = (lines flatMap { line => (line.seq map { _.loc -> line }) }).toMap

    def finished: Boolean = lines flatMap { _.seq filter { _.color.isDefined } } forall { spot => cells.byLoc(spot.loc) == spot.color }

    def moveLine(line: Line, movement: LineMovement): Option[Line] =
        if (movement.headMovement) {
            val head = line.seq.head
            val newHead = Spot(head.x + movement.dx, head.y + movement.dy, head.color)
            val newLoc = newHead.loc
            if ((cells.byLoc contains newLoc) &&
                (!(linesByLoc contains newLoc) ||
                    (linesByLoc(newLoc) == line && line.seq.size > 2 && line.seq.last.loc == newLoc))) {
                val newTail = line.seq zip line.seq.tail collect { case (a, b) => Spot(a.x, a.y, b.color) }
                Some(Line.of(line.name, newHead +: newTail))
            } else None
        } else {
            val last = line.seq.last
            val newLast = Spot(last.x + movement.dx, last.y + movement.dy, last.color)
            val newLoc = newLast.loc
            if ((cells.byLoc contains newLoc) &&
                (!(linesByLoc contains newLoc) ||
                    (linesByLoc(newLoc) == line && line.seq.size > 2 && line.seq.head.loc == newLoc))) {
                val newInit = line.seq.tail zip line.seq collect { case (a, b) => Spot(a.x, a.y, b.color) }
                Some(Line.of(line.name, newInit :+ newLast))
            } else None
        }

    def moveables: Map[(Line, LineMovement), Board] =
        (lines flatMap { line =>
            val otherLines = lines - line
            Board.movements flatMap { movement =>
                moveLine(line, movement) map { newLine => (line, movement) -> Board(cells, otherLines + newLine) }
            }
        }).toMap

    def lineMoved(line: Line, movement: LineMovement): Option[Board] = {
        val otherLines = lines - line
        moveLine(line, movement) map { newLine => Board(cells, otherLines + newLine) }
    }

    def printBoard(): Unit = {
        (cells.minY to cells.maxY) foreach { y =>
            (cells.minX to cells.maxX) foreach { x =>
                linesByLoc get (x, y) match {
                    case Some(line) if line.seq.head.loc == (x, y) => print(f"!${line.name}%1s")
                    case Some(line) if line.seq.last.loc == (x, y) => print(f"-${line.name}%1s")
                    case Some(line) => print(f"${line.name}%2s")
                    case None => print("  ")
                }
                cells.byLoc get (x, y) match {
                    case Some(Some(color)) => print(f"$color%2d")
                    case Some(None) => print(" .")
                    case None => print("  ")
                }
            }
            println()
        }
        println("----" * (cells.maxX - cells.minX + 1))
    }
}

case class Line(name: String, seq: Seq[Spot])
object Line {
    def of(name: String, seq: Seq[Spot]): Line = {
        val (head, last) = (seq.head.loc, seq.last.loc)
        if (head._1 < last._1 || (head._1 == last._1 && head._2 < last._2)) Line(name, seq)
        else Line(name, seq.reverse)
    }
}
case class LineMovement(headMovement: Boolean, dx: Int, dy: Int) {
    override def toString = (if (headMovement) "head" else "tail") + " " + ((dx, dy) match {
        case (-1, 0) => "left"
        case (1, 0) => "right"
        case (0, -1) => "up"
        case (0, 1) => "down"
    })
}
case class Spot(x: Int, y: Int, color: Option[Int]) {
    def loc = (x, y)
}
object Spot {
    def apply(x: Int, y: Int, color: Int) = new Spot(x, y, Some(color))
}
