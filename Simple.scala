
import collection.immutable.Queue

object Simple {

  type Path = List[Int]

  def path2pairs(path:Path):List[(Int,Int)] = path match {
    case Nil => Nil
    case List(x) => Nil
    case x1 :: x2 :: xs => (x1,x2) :: path2pairs(x2 :: xs)
  }

  trait Node {
    val id:Int
    val nodesOut:Seq[Int]
    val nodesIn:Seq[Int]
  }

  trait Graph[+N <: Node] {

    def node(id:Int):N
    def edges:Seq[(Int,Int)]
    def nodesOut(id:Int):Seq[Int]
    def nodesIn(id:Int):Seq[Int]

    def reachableFrom(startId:Int) =
      traverseFrom[List[N]](startId) (Nil) ((acc, n) => n :: acc)

    def traverseFrom[A](startId:Int)(a:A)(f: (A, N) => A):A = {

      @annotation.tailrec
      def helper(queue:Queue[N], acc:A, explored:Set[Int]):A = {
        if (queue.isEmpty) {
          acc
        } else {
          val (current, queue2) = queue.dequeue
          if (explored.contains(current.id)) {
            helper(queue2, acc, explored)
          } else {
            val acc2 = f(acc, current)
            val neighbours =
              nodesOut(current.id).map(n => node(n))

            helper(queue2 ++ neighbours, acc2, explored + current.id)
          }
        }
      }

      helper(Queue(node(startId)), a, Set())

    }

    def bfs(startId:Int, goalId:Int):Option[Path] = {

      @annotation.tailrec
      def helper(queue:Queue[Path], explored:Set[Int], goal:Int):Option[Path] = {
        if (queue.isEmpty) {
          None
        } else {
          val (path, queue2) = queue.dequeue
          val current = path.head
          if (explored.contains(current)) {
            helper(queue2, explored, goal)
          } else if (current == goal) {
            Some(path)
          } else {
            val neighbours =
              nodesOut(current)

            val newPaths = neighbours.map(_ :: path)
            helper(queue2 ++ newPaths, explored + current, goal)
          }
        }
      }

      if (startId == goalId) {
        Some(List())
      } else {
        val paths = nodesOut(startId).map(_ :: List(startId))
        val result = helper(Queue(paths: _*), Set(), goalId)
        result.map((p:Path) => assert (p.length >= 2))
        result.map(_.reverse)
      }
    }
  }

  case class FlowNode(id:Int, label:String, neighbours:List[Int] = Nil) extends Node {

    lazy val nodesOut = neighbours
    lazy val nodesIn = neighbours

    def addNeighbour(id:Int):FlowNode = copy(neighbours = (id :: neighbours).distinct)
  }

  trait FlowNetwork {
    val sourceId:Int
    val sinkId:Int
  }

  type Capacities = Map[(Int,Int),Int]
  type Flow = Map[(Int,Int),Int]

  case class RailNetwork(nodes:IndexedSeq[FlowNode], capacities:Capacities = Map[(Int,Int),Int](), flow:Flow = Map[(Int,Int),Int]())
             extends Graph[FlowNode] with FlowNetwork {

    def node(id:Int) = nodes(id)

    def edges:Seq[(Int,Int)] =
      nodes.flatMap(n => {
        nodesOut(n.id).map((n.id,_)) ++ nodesIn(n.id).map((_,n.id))
      }).distinct

    val sourceId = 0
    val sinkId = nodes.length - 1

    def nodesOut(id:Int) = nodes(id).nodesOut
    def nodesIn(id:Int) = nodes(id).nodesIn

    def capacity(a:Int, b:Int):Int =
      capacities.get((a,b)).orElse(capacities.get((b,a)))
        .map(c => if (c < 0) Integer.MAX_VALUE else c).get

    def flowFrom(a:Int,b:Int):Int =
      flow.get((a,b)).getOrElse(0)

    lazy val flowValue = nodesOut(sourceId).map(flowFrom(sourceId, _)).sum

    def toResidual:Residual = Residual(this)

    def addEdge(a:Int, b:Int, cap:Int):RailNetwork = {
      val an = node(a).addNeighbour(b)
      val bn = node(b).addNeighbour(a)
      val newCaps = capacities.updated((a,b), cap)
      copy(nodes = nodes.updated(a, an).updated(b, bn), capacities = newCaps)
    }

    def increaseFlow(ida:Int, idb:Int, inc:Int):RailNetwork = {
      val flowNow = flowFrom(ida, idb)
      val flow2 = flow.updated((ida, idb), flowNow + inc) // increase flow
      val flow3 = flow2.updated((idb, ida), -flow2((ida, idb))) // decrease flow in other dir
      copy(flow = flow3)
    }
    def minSTCut:STCut[FlowNode, Residual] = {
      val reachable = this.toResidual.reachableFrom(this.sourceId)
      STCut(reachable, this.nodes.diff(reachable), this.toResidual)
    }
  }

  object RailNetwork {
    def fromNodesAndEdges(nodes:IndexedSeq[FlowNode], edges:Seq[(Int,Int,Int)]):RailNetwork = {
      val initial = RailNetwork(nodes)
      val network = edges
        .foldLeft (initial) ((acc, edge) => edge match {
          case (from, to, c) => {
            acc.addEdge(from,to, c)
          }
        })
      network
    }
  }

  @annotation.tailrec
  final def maxFlow(nw:RailNetwork, c:Int = 1000):RailNetwork = {
    if (c <= 0) {
      nw
    } else {
      // nw.assertValid()
      val residual = nw.toResidual
      val pathOpt = residual.bfs(residual.sourceId, residual.sinkId)
      pathOpt match {
        case None => nw
        case Some(path) => {
          val nw2 = residual.augment(path, nw)
          maxFlow(nw2, c - 1)
        }
      }
    }
  }

  case class Residual(nw:RailNetwork) extends Graph[FlowNode] with FlowNetwork {

    def node(id:Int) = nw.nodes(id)

    def edges = nw.edges

    val sourceId = nw.sourceId
    val sinkId = nw.sinkId

    def nodesOut(id:Int) = node(id).nodesOut.filter(id2 => capacity(id, id2) > 0)
    def nodesIn(id:Int) = node(id).nodesIn.filter(id2 => capacity(id2, id) > 0)

    def capacity(a:Int, b:Int):Int = nw.capacity(a,b) - nw.flowFrom(a,b)

    def bottleneck(path:Path):Int = bottleneckpairs(path2pairs(path))

    def bottleneckpairs(lst:List[(Int,Int)]):Int =
      lst.map({ case (a,b) => capacity(a,b) }).filter(_ > 0).min

    def augment(path:Path, nw:RailNetwork):RailNetwork = {
      val pairs = path2pairs(path)
      val b = bottleneckpairs(pairs)
      pairs.foldLeft (nw) ((acc, e) => e match {
        case (from,to) => acc.increaseFlow(from, to, b)
      })
    }
  }

  case class STCut[+N <: Node, G <: Graph[N] with FlowNetwork](a:Seq[N], b:Seq[N], nw:G) {

    trait Part
    case object A extends Part
    case object B extends Part

    private lazy val aids = a.map(_.id)
    private lazy val bids = b.map(_.id)

    private def nodeId2part(id:Int):Part = {
      if (aids.contains(id)) {
        A
      } else if (bids.contains(id)) {
        B
      } else {
        throw new RuntimeException("All nodes should be in a partition")
      }
    }


    lazy val edgesOnCut =
      nw.edges.filter({case (a,b) => nodeId2part(a) != nodeId2part(b)})


    def assertValid():Unit = {
      for {
        afound <- a.find(n => n.id == nw.sourceId || n.id == nw.sinkId)
        bfound <- b.find(n => n.id == nw.sourceId || n.id == nw.sinkId)
      } yield {
        assert (afound.id != bfound.id)
      }
      assert(a.intersect(b) == Nil)
    }

    assertValid()

  }

}