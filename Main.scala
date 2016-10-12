import io.Source
import java.io.PrintWriter
import collection.immutable.Queue

object Main {

  def testNetwork = {
    val nodes = "s,a,b,c,d,e,f,g,t".split(",")
        .zipWithIndex.map(t => Node(t._2, t._1))
        .toIndexedSeq

    val edges = List(
      (0,1,12),(0,2,15),(0,3,20),(1,5,5),(1,6,2),(1,2,5),(2,3,11),(2,4,3),(2,6,6),
      (3,4,4),(3,7,8), (4,6,6),(4,7,1),(5,8,18),(5,6,9),(6,7,7),(6,8,13),(7,8,10)
    ).zipWithIndex.map({ case ((f,t,c),i) => UndEdge(i,f,t,c)})

    Network(nodes, edges)
  }

  def main(args:Array[String]):Unit = {

    val Some(network) = parseNetwork("./data/rail.txt")
    val residual = network.toResidual
    val Some(path) = residual.bfs(residual.sourceId, residual.sinkId)
    val maxFlowNw = FlowNetwork.maxFlow(network, 100)
    new PrintWriter("graph.dot") {
      write(maxFlowNw.toGraphViz("graph")); close
    }
    new PrintWriter("residual.dot") {
      write(maxFlowNw.toResidual.toGraphViz("digraph")); close
    }
    println(maxFlowNw.flow.map)
    println("max flow: " + maxFlowNw.flow.value)
    val reachable = maxFlowNw.toResidual.reachableFrom(maxFlowNw.sourceId)
    // println(reachable)
    val mincut = STCut(reachable, maxFlowNw.nodes.diff(reachable), maxFlowNw.toResidual)
    val edges = mincut.edgesOnCut
    println(edges.sortBy(_.to).map(e => s"${e.to} ${e.from} ${e.capacity}").mkString("\n"))

  }

  type Path = List[Edge]

  case class STCut(a:Seq[Node], b:Seq[Node], nw:FlowNetwork) {

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

    lazy val edgesOnCut:Seq[Edge] =
      nw.edges.filter(e => nodeId2part(e.from) != nodeId2part(e.to))


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

  def path2string(path:Path, nw:FlowNetwork):String = {
    nw.node(path.last.from).label +
      path.map(e => s" =${e.flow}/${e.capacity}=> " + nw.node(e.to).label).reverse.mkString

  }

  def bottleneck(path:Path):Int = {
    path.filter(_.hasFiniteCapacity).map(e => e.capacity - e.flow).min
  }

  case class Flow(map:Map[(Int,Int), Int] = Map()) {

    def value(ida:Int, idb:Int):Int = {
      map.get((ida,idb)).getOrElse(0)
    }

    map.foreach({case ((a,b),v) => assert (-v == map((b,a)))})

    def value:Int = flowOut(0).map(_._2).sum

    def updated(ida:Int, idb:Int, flow:Int):Flow = {
      val flowNow = value(ida, idb)
      val map2 = map.updated((ida, idb), flowNow + flow) // increase flow
      val map3 = map2.updated((idb, ida), -map2((ida, idb))) // decrease flow in other dir
      copy(map = map3)
    }

    def flowOut(id:Int) = map.keys.filter(_._1 == id).map(k => (k,map(k)))
      .filter(_._2 > 0)

    def flowIn(id:Int) = map.keys.filter(_._2 == id).map(k => (k,map(k)))
      .filter(_._2 > 0)
  }

  trait Graph {

    val nodes:Seq[Node]
    val edges:Seq[Edge]

    def node(id:Int) = nodes(id)
    def edge(index:Int) = edges(index)
    def edgesFrom(nodeId:Int) = edges.filter(_.from == nodeId)
    def edgesTo(nodeId:Int) = edges.filter(_.to == nodeId)

    def reachableFrom(startId:Int) =
      traverseFrom[List[Node]](startId) (Nil) ((acc, n) => n :: acc)

    def traverseFrom[A](startId:Int)(a:A)(f: (A, Node) => A):A = {

      @annotation.tailrec
      def helper(queue:Queue[Node], acc:A, explored:Set[Int]):A = {
        if (queue.isEmpty) {
          acc
        } else {
          val (current, queue2) = queue.dequeue
          if (explored.contains(current.id)) {
            helper(queue2, acc, explored)
          } else {
            val acc2 = f(acc, current)
            val neighbours =
              edgesFrom(current.id).map(e => node(e.to))

            helper(queue2 ++ neighbours, acc2, explored + current.id)
          }
        }
      }

      helper(Queue(node(startId)), a, Set())

    }

    def bfs(startId:Int, goalId:Int):Option[Path] = {

      val start = node(startId)
      val goal  = node(goalId)

      @annotation.tailrec
      def helper(queue:Queue[Path], explored:Set[Int], goal:Node):Option[Path] = {
        if (queue.isEmpty) {
          None
        } else {
          val (path, queue2) = queue.dequeue
          val edge = path.head
          val current = node(edge.to)
          if (explored.contains(current.id)) {
            helper(queue2, explored, goal)
          } else if (current.id == goal.id) {
            Some(path)
          } else {
            // filter is not necessary, but speeds up a little bit (in theory)
            val neighbours =
              edgesFrom(current.id)

            val newPaths = neighbours.map(_ :: path)
            helper(queue2 ++ newPaths, explored + current.id, goal)
          }
        }
      }

      if (start.id == goal.id) {
        Some(List())
      } else {
        val paths = edgesFrom(start.id).map(List(_))
        helper(Queue(paths: _*), Set(), goal)
      }
    }

  }

  trait UndGraph extends Graph {

    override def edgesFrom(nid:Int) = super.edgesFrom(nid) ++
      super.edgesTo(nid).map(_.reverse)

    override def edgesTo(nid:Int) = super.edgesTo(nid) ++
      super.edgesFrom(nid).map(_.reverse)
  }

  object FlowNetwork {

    @annotation.tailrec
    def maxFlow(nw:Network, c:Int = 1000):Network = {
      if (c <= 0) {
        nw
      } else {
        nw.assertValid()
        val residual = nw.toResidual
        residual.assertValid()
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

  }

  trait FlowNetwork extends Graph {
    lazy val source = nodes.head
    lazy val sink = nodes.last
    lazy val internal = nodes.tail.inits
    lazy val sourceId = source.id
    lazy val sinkId = sink.id

    val flow:Flow

    def assertValid():Unit = {
      // println("Asserting for " + this.getClass)
      edges.foreach(e => if (e.hasFiniteCapacity) assert(e.flow <= e.capacity))
    }


  }

  case class Network(nodes:IndexedSeq[Node], edges:Seq[UndEdge], flow:Flow = Flow())
             extends FlowNetwork with UndGraph {

    def toResidual:Residual = Residual(this)

    def edge(aid:Int, bid:Int):Option[UndEdge] = {
      assert (aid != bid)
      println(aid, bid)
      edges.find(e => e.from == aid && e.to == bid || e.to == aid && e.from == bid)
    }

    def hasEdge(aid:Int, bid:Int) = !edge(aid, bid).isEmpty

    def flow(aid:Int, bid:Int):Int = flow.value(aid, bid)

    def increaseFlow(aid:Int, bid:Int, value:Int):Network = {
      val newflow = flow.updated(aid, bid, value)
      copy(flow = newflow)

    }

    def toGraphViz(gt:String = "graph"):String = {
      def e2s(e:Edge):String = e match {
        case UndEdge(id, from, to, cap, _) => {
          val flowf:Int = this.flow(from,to)
          val flowb:Int = this.flow(to,from)
          assert (flowf + flowb == 0)

          val color =
            if (flowf != 0)
              "green"
            else "black"
          val flowStyle =
            if (flowf > 0) {
              """,arrowhead="halfopen",arrowtail="none",dir="both""""
            } else if (flowf < 0) {
              """,arrowtail="halfopen",arrowhead="none",dir="both""""
            } else ""

          s"""$from -- $to [label="${Math.abs(flowf)}/$cap", color=$color, len=3 $flowStyle]"""
        }
      }
      val nodesStr = nodes.map(n => n.id + " [label=\"" + n.label + "\"]").mkString("\n")
      val edgesStr = edges
        .map(e2s(_))
        .mkString("\n")

      s"""$gt Network {
        rankdir=LR
        ${nodesStr}
        ${edgesStr}}
      """
    }


  }

  case class Residual(nw:Network) extends FlowNetwork with Graph {

    lazy val edges = nw.edges.flatMap(e => {
      val cap = e.capacity
      val cap2 = if (cap < 0) Integer.MAX_VALUE else cap
      val e1 = DirEdge(0, e.from, e.to, cap2 - nw.flow(e.from, e.to))
      val e2 = DirEdge(0, e.to, e.from, cap2 - nw.flow(e.to, e.from))
      // assert(e1.capacity >= 0)
      // assert(e2.capacity >= 0)
      List(e1,e2).filter(_.capacity > 0)
    })

    val nodes = nw.nodes
    val flow = nw.flow

    def augment(path:Path, nw:Network):Network = {
      val b = bottleneck(path)
      println("augment: " + path2string(path, nw) + " with " + b)
      val r = path.foldLeft (nw) ((acc, e) => e match {
        case DirEdge(id, from, to, cap) => acc.increaseFlow(from, to, b)
        case _ => throw new RuntimeException("Only directed edges on path allowed")
      })

      for (u <- r.nodes/*; v <- nodes if u.id != v.id && hasEdge(u.id,v.id)*/) {

        val flowOut = r.flow.flowOut(u.id)
        val flowIn  = r.flow.flowIn(u.id)

        // val flowIn  = edgesTo(u.id).map(e => flow(e.from, e.to))
        println(s"${u.id} in: $flowIn, out: $flowOut")
      }

      r

    }

    def toGraphViz(gt:String = "digraph"):String = {
      def e2s(e:Edge):String = e match {
        case e@DirEdge(id, from, to, cap) => {
          s"""$from -> $to [label="$cap", len=3]"""
        }
      }
      val nodesStr = nodes.map(n => n.id + " [label=\"" + n.label + "\"]").mkString("\n")
      val edgesStr = edges
        .map(e2s(_))
        .mkString("\n")

      s"""$gt Network {
        rankdir=LR
        ${nodesStr}
        ${edgesStr}}
      """
    }
  }

  object FunIterator {

    def pure[A]:FunIterator[A,Unit] = FunIterator(seq => Some(((), seq)))

    def head[A]:FunIterator[A,A] = FunIterator(seq => seq.headOption.map((_, seq.tail)))

    def take[A](n:Int):FunIterator[A,Seq[A]] =
      FunIterator(seq => Some((seq.take(n), seq.drop(n))))

    def isEmpty[A]:FunIterator[A,Boolean] =
      FunIterator(seq => Some((seq.isEmpty, seq)))


  }
  case class FunIterator[A,B](run: Seq[A] => Option[(B, Seq[A])]) {

    def map[C](f:B => C):FunIterator[A,C] =
      FunIterator(seq => {
        this.run(seq).map({case (x, more) => (f(x), more)})
      })

    def flatMap[C](f: B => FunIterator[A,C]):FunIterator[A,C] = {
      FunIterator(seq => {
        this.run(seq).flatMap({case (x, more) => f(x).run(more)})
      })
    }


  }

  case class Node(id:Int, label:String)

  trait Edge {
    val id:Int
    val from:Int
    val to:Int
    val capacity:Int
    val flow:Int
    val isUndirected:Boolean
    val isBackwards:Boolean = !isUndirected
    val asDirected:Seq[DirEdge]

    // assert(capacity >= -1)

    def reverse:Edge

    def hasMoreCapacity:Boolean = (capacity < 0 || capacity > flow)
    def hasFiniteCapacity:Boolean = capacity > 0

    def toString(nw:Network):String = {
      val f = nw.node(from).label
      val t = nw.node(to).label
      s"${f} --> ${t}"
    }
  }

  // Undirected Edge
  case class UndEdge(id:Int, from:Int, to:Int, capacity:Int, flow:Int = 0) extends Edge {
    val isUndirected = true
    def reverse = DirEdge(id, from = to, to = from, capacity = capacity)
    assert (capacity == -1 || flow <= capacity)

    lazy val asDirected = List(DirEdge(id,from,to,capacity), reverse)
  }
  case class DirEdge(id:Int, from:Int, to:Int, capacity:Int) extends Edge {
    def reverse = copy(from = to, to = from)
    val isUndirected = false
    lazy val asDirected = List(this)
    val flow = 0
  }


  case class BEdge(edge:Edge) extends Edge {
    val id = edge.id * -1 // not really necessary
    val from = edge.to
    val to   = edge.from
    val capacity = edge.flow

    assert (capacity > 0)

    val flow = 0
    val isUndirected = false
    def reverse = copy(edge.reverse)
    lazy val asDirected = edge.asDirected.reverse
  }

  def parseNetwork(filepath:String):Option[Network] = {

    val src = Source.fromFile(filepath)
    import FunIterator._

    try {
      val iter:FunIterator[String,Network] = for {
        nNodes <- head
        nodes <- take(nNodes.toInt)
        nEdges <- head
        edges <- take(nEdges.toInt)
        done <- isEmpty
      } yield {
        assert (done == true)
        val nodesSeq = nodes.zipWithIndex.map({case (lbl,i) => Node(i, lbl)}).toIndexedSeq
        val edgesSeq = edges.map(_.split(" ").map(_.toInt))
          .zipWithIndex
          .map({case (Array(from,to,cap), id) => UndEdge(id, from, to, cap)})
        Network(nodesSeq, edgesSeq)
      }
      val result = iter.run(src.getLines.toList)
      // println(result)
      result.map(_._1)
    } finally {
      src.close()
    }
  }

}