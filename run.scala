import java.nio.file.*
import java.io.*

@main def run(file: String): Unit =
  val in = Files.newInputStream(Paths.get(file))

  Graph.load(BufferedInputStream(in)) match {
    case Left(m) => throw IllegalArgumentException(s"""Failed to read input: ${m.mkString(", ")}""")
    case Right(g) =>
      val (stations, path) = Graph.process(g, Node(1))

      println(stations)
      println(path.map(g => s"Station $g").mkString(" --> "))
  }
