package quetsolver

import scala.collection.parallel.ParSeq
import scala.Enumeration

object Stages {
    def stage1_11 = {
        object Colors extends Enumeration {
            val R = Value(1)
            val B = Value(2)
        }
        import Colors._
        Board(
            Cells(((1 to 4) map { spot(_, 3) }).toSet ++ ((1 to 4) map { spot(_, 4) }).toSet + spot(1, 2) + spot(4, 2) + spot(1, 1, R) + spot(4, 1, B)),
            Set(Line.of("A", spot(1, 1, B) +: ((2 to 4) map { spot(1, _) })),
                Line.of("B", spot(4, 1, R) +: ((2 to 4) map { spot(4, _) }))))
    }

    def stage7_12 = {
        object Colors extends Enumeration {
            val G = Value(1)
            val R = Value(2)
            val B = Value(3)
            val Y = Value(4)
        }
        import Colors._

        Board(
            Cells(
                ((1 to 7) flatMap { x => (1 to 2) map { y => spot(x, y, G) } }).toSet ++
                    ((2 to 6) flatMap { x => (6 to 7) map { y => spot(x, y, R) } }).toSet ++
                    ((3 to 8) map { spot(1, _, Y) }) ++ ((3 to 8) map { spot(7, _, Y) }) ++ ((2 to 6 map { spot(_, 8, Y) })) ++
                    ((2 to 6) map { spot(_, 3, B) }) ++ ((2 to 6) map { spot(_, 5, B) }) + spot(2, 4, B) + spot(4, 4, B) + spot(6, 4, B) +
                    Spot(3, 4, None) + Spot(5, 4, None)),
            Set(Line.of("B", (spot(1, 2, B) +: ((1 to 7) map { spot(_, 1, B) })) ++ ((2 to 6) map { spot(7, _, B) })),
                Line.of("Y", ((2 to 6) map { spot(_, 2, Y) }) ++ ((6 to 3 by -1) map { spot(_, 3, Y) }) ++ ((3 to 6) map { spot(_, 4, Y) }) ++ ((6 to 3 by -1) map { spot(_, 5, Y) })),
                Line.of("G", ((5 to 3 by -1) map { spot(2, _, G) }) ++ ((3 to 8) map { spot(1, _, G) }) ++ ((2 to 6) map { spot(_, 8, G) })),
                Line.of("R", ((5 to 2 by -1) map { spot(_, 7, R) }) ++ ((2 to 6) map { spot(_, 6, R) }) :+ spot(6, 7, R))))
    }

    def spot(x: Int, y: Int): Spot = Spot(x, y, None)
    def spot[T <: Enumeration#Value](x: Int, y: Int, color: T): Spot = Spot(x, y, color.id)

    def stage11_1 = {
        object Colors extends Enumeration {
            val G = Value(1)
            val B = Value(2)
            val Y = Value(3)
            val O = Value(4)
        }
        import Colors._

        Board(
            Cells(Set(
                spot(2, 1), spot(5, 1),
                spot(2, 2), spot(3, 2, B), spot(4, 2, B), spot(5, 2),
                spot(2, 3), spot(3, 3, B), spot(4, 3, B), spot(5, 3),
                spot(1, 4), spot(2, 4, Y), spot(3, 4, O), spot(4, 4, O), spot(5, 4, Y), spot(6, 4),
                spot(1, 5), spot(2, 5, O), spot(3, 5, Y), spot(4, 5, Y), spot(5, 5, O), spot(6, 5),
                spot(2, 6), spot(3, 6, G), spot(4, 6, G), spot(5, 6),
                spot(2, 7), spot(3, 7), spot(4, 7), spot(5, 7),
                spot(2, 8), spot(5, 8))),
            Set(Line.of("A", Seq(spot(2, 1, G), spot(2, 2), spot(2, 3), spot(2, 4))),
                Line.of("B", Seq(spot(5, 1, G), spot(5, 2), spot(5, 3), spot(5, 4))),
                Line.of("C", Seq(spot(1, 4, Y), spot(1, 5, O), spot(2, 5))),
                Line.of("D", Seq(spot(6, 4, Y), spot(6, 5, O), spot(5, 5))),
                Line.of("E", Seq(spot(3, 3), spot(3, 4, Y), spot(3, 5, O))),
                Line.of("F", Seq(spot(4, 3), spot(4, 4, Y), spot(4, 5, O))),
                Line.of("G", Seq(spot(3, 6), spot(3, 7), spot(2, 7), spot(2, 8, B))),
                Line.of("H", Seq(spot(4, 6), spot(4, 7), spot(5, 7), spot(5, 8, B)))))
    }

    def stage11_10 = {
        object Colors extends Enumeration {
            val G = Value(1)
            val B = Value(2)
            val R = Value(3)
            val Y = Value(4)
        }
        import Colors._

        Board(
            Cells(
                ((1 to 5) map { spot(1, _, B) }).toSet + spot(2, 1, B) + spot(3, 1, B) ++
                    ((1 to 5) map { spot(6, _, G) }).toSet + spot(4, 1, G) + spot(5, 1, G) ++
                    ((2 to 5) map { Spot(_, 3, None) }) ++
                    Set(Spot(3, 4, None), Spot(4, 4, None), Spot(1, 6, None), Spot(6, 6, None),
                        spot(2, 6, Y), spot(3, 6, Y), spot(4, 6, R), spot(5, 6, R), Spot(2, 7, None), Spot(4, 7, None))),
            Set(Line.of("A", (((5 to 1 by -1) map { spot(1, _, G) }).toSeq :+ spot(2, 1, G) :+ spot(3, 1, G) :+ spot(4, 1, B) :+ spot(5, 1, B)) ++ ((1 to 5) map { spot(6, _, B) })),
                Line.of("R", Seq(spot(2, 6, R), spot(3, 6, R))),
                Line.of("Y", Seq(spot(4, 6, Y), spot(5, 6, Y)))))
    }
}

object Main {

    def main(args: Array[String]): Unit = {
        val initialBoard = Stages.stage7_12

        initialBoard.printBoard()
        // initialBoard.moveables map { _._2.printBoard() }
        solve()
        def solve(): Unit = {
            val startTime = System.currentTimeMillis()
            val result = pbfs(initialBoard)
            println(s"elapsed time: ${System.currentTimeMillis() - startTime} millis")
            result match {
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
    }

    def bfs(initialBoard: Board): Option[(List[(Line, LineMovement)], Board)] = bfs(List((List(), initialBoard)), scala.collection.mutable.Set(initialBoard))
    def bfs(queue: List[(List[(Line, LineMovement)], Board)], memo: scala.collection.mutable.Set[Board]): Option[(List[(Line, LineMovement)], Board)] = {
        queue match {
            case (path, head) +: rest =>
                if (head.finished) {
                    Some((path, head))
                } else {
                    var nextQueue: List[(List[(Line, LineMovement)], Board)] = rest
                    head.moveables foreach { m =>
                        val move = m._1
                        val board = m._2
                        memo.synchronized {
                            if (!(memo contains board)) {
                                memo += board
                                println(memo.size)
                                nextQueue :+= (move +: path, board)
                            }
                        }
                    }
                    bfs(nextQueue, memo)
                }
            case List() => None
        }
    }

    def pbfs(initialBoard: Board): Option[(List[(Line, LineMovement)], Board)] = pbfs(1, List((List(), initialBoard)), scala.collection.mutable.Set(initialBoard.lines))
    def pbfs(it: Int, queue: List[(List[(Line, LineMovement)], Board)], memo: scala.collection.mutable.Set[Set[Line]]): Option[(List[(Line, LineMovement)], Board)] = {
        trait ParResult
        case class Done(result: (List[(Line, LineMovement)], Board)) extends ParResult
        case object Todo extends ParResult
        if (it % 50 == 0) {
            println(memo.size, queue.size)
        }
        val (par, rest) = (queue take 100, queue drop 100)
        var newQueue: List[(List[(Line, LineMovement)], Board)] = List()
        val processed: ParSeq[ParResult] = par.par map {
            _ match {
                case (path, head) =>
                    if (head.finished) {
                        Done((path, head))
                    } else {
                        head.moveables foreach { m =>
                            val move = m._1
                            val board = m._2
                            memo.synchronized {
                                if (!(memo contains board.lines)) {
                                    memo += board.lines
                                    newQueue :+= (move +: path, board)
                                }
                            }
                        }
                        Todo
                    }
            }
        }
        val done = processed find { _.isInstanceOf[Done] }
        if (done.isEmpty) {
            pbfs(it + 1, rest ++ newQueue, memo)
        } else {
            done.get match { case Done(result) => Some(result) }
        }
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
