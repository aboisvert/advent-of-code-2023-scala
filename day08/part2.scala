package day08

@main def part2 =
  val lines = scala.io.Source
    .fromFile("day08/input.txt")
    .getLines
  val (instructions, network) = parseMap(lines)
  import SmartyPants.*
  println(instructions.pathLengthAsGhost(using network))

object SmartyPants:
  def lcm(a: BigInt, b: BigInt): BigInt =
      (a * b).abs / a.gcd(b)
  extension (instr: Instructions)
    def pathLengthAsGhost(using network: Network): BigInt =
      val startingNodes = network.nodes.values.filter(_.name endsWith "A")
      startingNodes
        .map: node =>
          println(node)
          val len = instr.pathLength(startNode = node, endCond = _.name endsWith "Z")
          println(len)
          BigInt(len)
        .reduce(lcm(_, _))
    end pathLengthAsGhost

object BruteForce:
  extension (instr: Instructions)
    def pathLengthAsGhost(startNode: Node)(using network: Network): Long =
      val startingNodes = network.nodes.values.filter(_.name endsWith "A")

      case class State(distance: Long, nodes: Iterable[Node])
      val endState = Iterator
        .continually(instr.path.iterator)
        .flatten
        .foldLeftWhile(
          State(distance = 0, startingNodes),
          cond = !_.nodes.forall(_.name endsWith "Z")
        ): (state, instruction) =>
          if state.distance % 1000000 == 0 then
            println(state.distance)
          val nextNodes = state.nodes.map: node =>
            instruction match
              case 'L' => node.leftNode
              case 'R' => node.rightNode
          State(state.distance + 1, nextNodes)
      endState.distance
    end pathLengthAsGhost
