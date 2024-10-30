import zio.prelude.*

import cats.implicits.*
import scala.util.control.NonFatal

import zio.*

type Stations = Set[Node]

case class Graph(fuelingStations: Stations, connections: Map[Node, Map[Node, Int]], initialCapacity: Int)

type Node = Node.Type
object Node extends Newtype[Int]

case class Edge(node: Node, weight: Int)

trait Context {
  extension (n: Node) def edges: Option[NonEmptyList[Edge]]
  extension (n: Node) def move(to: Edge): Boolean
  extension (n: Node) def previous: Option[Node]
  extension (n: Node) def isFuelingStation: Boolean
  extension (n: Node) def path(to: Node): List[Node]

  extension (n: Node) def getNextNode: Option[Edge]

}

object Context {
  def init(g: Graph, startNode: Node): Context = new Context {
    extension (n: Node)
      def edges: Option[NonEmptyList[Edge]] = NonEmptyList.fromIterableOption {
        g.connections.get(n).fold(Nil) {
          _.toList.map { (n, w) =>
            Edge(n, w)
          }
        }
      }

    extension (n: Node) def previous: Option[Node] = _previouses.get(n)

    extension (n: Node)
      def getNextNode: Option[Edge] =
        n.edges.map {
          _.minBy { case Edge(_, weight) => weight }

        }

    extension (n: Node)
      def move(to: Edge): Boolean =
        var moved = false

        if !_visited.contains(to.node) then {
          val _newCap = if to.node.isFuelingStation then g.initialCapacity else _capacity - to.weight

          if _newCap >= 0 then {
            _capacity = _newCap
            _previouses(to.node) = n
            _visited += to.node
            moved = true
          }

        }

        moved

    extension (n: Node) def isFuelingStation: Boolean = g.fuelingStations.contains(n)

    extension (n: Node)
      def path(to: Node): List[Node] =

        @annotation.tailrec
        def go(node: Option[Node], acc: List[Node] = Nil): List[Node] = node match {
          case Some(`to`)  => to :: acc
          case None        => List()
          case Some(other) => go(other.previous, other :: acc)
        }

        go(n.some)

    private val _previouses = scala.collection.mutable.Map[Node, Node]()
    private var _visited    = Set.empty[Node] + startNode
    private var _capacity   = g.initialCapacity
  }

}

object Graph {
  import java.io.*

  enum State {
    case start()
    case stations(args: State.Args)
    case connections(args: State.Args, fuelingStations: Stations)

  }

  object State {
    case class Args(locations: Int, roads: Int, capacity: Int)

    object Args {
      def validateNonNegative(value: Int, tag: "locations" | "roads" | "capacity" | "position" | "row" | "weight") =
        Validation.fromPredicateWith(s"$tag $value is negative")(value)(_ > 0)

      def validateArgs(a: State.Args) =
        Validation.validateWith(
          validateNonNegative(a.locations, tag = "locations"),
          validateNonNegative(a.roads, tag = "roads"),
          validateNonNegative(a.capacity, tag = "capacity")
        )(State.Args.apply)

    }
  }

  def load(in: InputStream): Either[NonEmptyChunk[String], Graph] =

    def go(s: State, reader: BufferedReader): Either[NonEmptyChunk[String], Graph] =
      s match {
        case State.start() =>
          try {
            reader.readLine.trim.split(" ").map(_.toInt) match {
              case Array(locations, roads, capacity) =>
                val args = State.Args(locations, roads, capacity)

                State.Args.validateArgs(args).fold(failure = e => Left(e), success = a => go(State.stations(a), reader))

              case _ => Left(NonEmptyChunk(s"Error reading arguments"))
            }
          } catch {
            case NonFatal(e) => Left(NonEmptyChunk(e.getMessage))

          }

        case State.stations(args @ State.Args(locations, roads, l)) =>
          try {
            val line = reader.readLine.trim.toList

            if line.length != locations then
              Left(NonEmptyChunk(s"Invalid stations length: ${line.length}, needs $locations entries"))
            else {

              val fuelingStations = line.zipWithIndex.foldLeft(Set.empty[Node]) {
                case (set, ('1', i)) => set + Node(i + 1)
                case (set, ('0', _)) => set
                case (_, (v, i)) =>
                  throw Exception(
                    s"Invalid gas station $v at location $i, should be '0' or '1'"
                  )
              }

              val s = State.connections(args, fuelingStations = fuelingStations)

              go(s, reader)

            }
          } catch {
            case NonFatal(e) => Left(NonEmptyChunk(e.getMessage))

          }

        case State.connections(State.Args(locations, roads, capacity), fuelingStations) =>
          try {
            val z = scala.collection.mutable.Map.empty[Node, Map[Node, Int]].withDefaultValue(Map())

            val connections = (0 until roads).foldLeft(z) { case (connections, row) =>
              reader.readLine.trim.split(" ").map(_.toInt) match {
                case Array(u, v, weight) =>
                  val validatedConnections = Validation.validate(
                    State.Args.validateNonNegative(u, tag = "position"),
                    State.Args.validateNonNegative(v, tag = "row"),
                    State.Args.validateNonNegative(weight, tag = "weight")
                  )

                  validatedConnections.fold(
                    failure = e => throw IllegalArgumentException(e.mkString(", ")),
                    success = { case (u, v, weight) =>
                      val map = connections(Node(u))
                      connections.update(Node(u), map + (Node(v) -> weight))

                      connections
                    }
                  )

                case v =>
                  throw Exception(s"Invalid connection $v at location ${row + 1}")

              }

            }

            Right(Graph(fuelingStations, connections.toMap, capacity))
          } catch {
            case NonFatal(e) => Left(NonEmptyChunk(e.getMessage))

          }

      }

    go(State.start(), reader = BufferedReader(InputStreamReader(in)))

  def process(g: Graph, startNode: Node): (Int, List[Node]) =

    given Context = Context.init(g, startNode)

    @annotation.tailrec
    def go(node: Node, stations: Int): (Int, Node) =
      node.getNextNode match {
        case Some(edge) =>
          if node.move(to = edge) then go(edge.node, stations + 1) else (stations, node)

        case None =>
          (stations, node)
      }

    val (stations, lastNode) = go(startNode, 1)

    (stations, lastNode.path(to = startNode))

}
