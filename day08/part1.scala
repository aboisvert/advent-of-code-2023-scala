package day08

import scala.util.boundary

@main def part1 =
  val lines = scala.io.Source
    .fromFile("day08/input.txt")
    .getLines
  val (instructions, network) = parseMap(lines)
  val length = instructions.pathLength(
    network("AAA"),
    endCond = _.name == "ZZZ"
  )(using network)
  println(length)

case class Network(nodes: Map[String, Node]):
  export nodes.apply

case class Instructions(path: String):
  val startNode = "AAA"
  val endNode = "ZZZ"

  def pathLength(startNode: Node, endCond: Node => Boolean)(using
      network: Network
  ): Int =
    case class State(distance: Int, node: Node)
    val endState = Iterator
      .continually(path.iterator)
      .flatten
      .foldLeftWhile(
        State(distance = 0, startNode),
        cond = (st: State) => !endCond(st.node)
      ): (state, instruction) =>
        val nextNode = instruction match
          case 'L' => state.node.leftNode
          case 'R' => state.node.rightNode
        State(state.distance + 1, nextNode)
    endState.distance
  end pathLength
end Instructions

case class Node(name: String, left: String, right: String):
  def leftNode(using network: Network) = network(left)
  def rightNode(using network: Network) = network(right)

def parseMap(lines: Iterator[String]): (Instructions, Network) =
  val instructions = lines.next()
  lines.next()
  val nodes = lines.map(parseNode)
  (Instructions(instructions), Network(nodes.map(n => (n.name, n)).toMap))

val NodeRegex = """(\w+) = \((\w+), (\w+)\)\s*""".r
def parseNode(line: String): Node =
  line match
    case NodeRegex(name, left, right) => Node(name, left, right)

extension [T](iter: Iterator[T])
  def foldLeftWhile[ACC](acc: ACC, cond: ACC => Boolean)(
      op: (ACC, T) => ACC
  ): ACC =
    boundary:
      iter.foldLeft(acc): (b, a) =>
        val result = op(b, a)
        if !cond(result) then boundary.break(result)
        result
  end foldLeftWhile
