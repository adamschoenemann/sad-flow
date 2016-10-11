import io.Source
import java.io.PrintWriter
import collection.immutable.Queue

object Main {


  def main(args:Array[String]):Unit = {

    val Some(network) = parseNetwork("./data/rail.txt")
    val residual = network.toResidual
    val Some(path) = network.bfs(network.sourceId, network.sinkId)
    val Some(path2) = residual.bfs(residual.sourceId, residual.sinkId)

    val maxFlowNw = FlowNetwork.maxFlow(network)
    println("Final flow: " + maxFlowNw.flow)
    val reachable = maxFlowNw.toResidual.reachableFrom(maxFlowNw.sourceId)
    // println(reachable)
    val mincut = STCut(reachable, maxFlowNw.nodes.diff(reachable), maxFlowNw.toResidual)
    val edges = mincut.edgesOnCut
    println(edges.map(e => s"${e.from} ${e.to} ${e.capacity}").mkString("\n"))
    // println(path2string(path2, residual))
    new PrintWriter("graph.dot") { write(maxFlowNw.toGraphViz); close }
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

  trait Graph {

    val nodes:Seq[Node]
    val edges:Seq[Edge]

    def node(id:Int) = nodes(id)
    def edge(index:Int) = edges(index)
    def edgesFrom(nodeId:Int) = edges.filter(_.from == nodeId)
    def edgesTo(nodeId:Int) = edges.filter(_.to == nodeId)
    def edgesAsDirected(nodeId:Int) =
      (edgesFrom(nodeId) ++ edgesTo(nodeId)).flatMap(_.asDirected)

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
              edgesAsDirected(current.id).map(e => node(e.to))

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
              edgesAsDirected(current.id).filter(e => !explored.contains(e.to))

            val newPaths = neighbours.map(_ :: path)
            helper(queue2 ++ newPaths, explored + current.id, goal)
          }
        }
      }

      if (start.id == goal.id) {
        Some(List())
      } else {
        val paths = edgesAsDirected(start.id).map(List(_))
        helper(Queue(paths: _*), Set(), goal)
      }
    }

    lazy val toGraphViz:String = {
      def e2s(e:Edge):String = e match {
        case UndEdge(id, from, to, cap, flow) if flow > 0 => {
          val color = if (flow > 0) "green" else "black"
          s"""$from -> $to [label="$flow/$cap", color=$color, dir="both"]"""
        }
        case b@BEdge(_) => {
          s"${b.from} -> ${b.to} [label=${b.capacity}, style=dashed]"
        }
        case _ => ""
      }
      val nodesStr = nodes.map(n => n.id + " [label=\"" + n.label + "\"]").mkString("\n")
      val edgesStr = edges
        .map(e2s(_))
        .mkString("\n")

      s"""digraph Network {
        rankdir=LR
        splines=true
        overlap=false
        ${nodesStr}
        ${edgesStr}}
      """
    }
  }

  object FlowNetwork {

    @annotation.tailrec
    def maxFlow(nw:Network):Network = {
      nw.assertValid()
      val residual = nw.toResidual
      residual.assertValid()
      val pathOpt = residual.bfs(residual.sourceId, residual.sinkId)
      pathOpt match {
        case None => nw
        case Some(path) => {
          val nw2 = residual.augment(path, nw)
          maxFlow(nw2)
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

    lazy val flow = edgesTo(sinkId).map(_.flow).sum

    def assertValid():Unit = {
      // println("Asserting for " + this.getClass)
      edges.foreach(e => if (e.hasFiniteCapacity) assert(e.flow <= e.capacity))
    }
  }

  case class Network(nodes:IndexedSeq[Node], edges:Seq[UndEdge]) extends FlowNetwork {

    def toResidual:Residual = Residual(nodes, edges)



    def increaseFlow(edgeId:Int, delta:Int):Network = {
      val e = edge(edgeId)
      val newEdge = e.asInstanceOf[UndEdge].copy(flow = e.flow + delta)
      if (newEdge.capacity >= 0 && newEdge.flow > newEdge.capacity) {
        throw new RuntimeException(s"Edge ${edgeId} flow is ${newEdge.flow} but " +
          s"capacity is only ${newEdge.capacity}")
      }
      copy(edges = edges.updated(edgeId, newEdge))
    }

    def decreaseFlow(edgeId:Int, delta:Int):Network =
      increaseFlow(edgeId, delta  * -1)

  }

  case class Residual(nodes:IndexedSeq[Node], forwards:Seq[UndEdge]) extends FlowNetwork {

    lazy val backwards:Seq[BEdge] =
      forwards.filter(e => e.flow > 0).map(e => BEdge(e))

    lazy val edges = forwards.filter(e => e.hasMoreCapacity) ++ backwards

    def augment(path:Path, nw:Network):Network = {
      val b = bottleneck(path)
      path.foldLeft (nw) ((acc, e) => e match {
        case UndEdge(id, from, to, cap, flow) => acc.increaseFlow(id, b)
        case BEdge(e2) => acc.decreaseFlow(e2.id, b)
      })
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
    val asDirected:Seq[Edge]

    assert(capacity >= -1)

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
    def reverse = copy(from = to, to = from)
    assert (capacity == -1 || flow <= capacity)

    lazy val asDirected = List(this, reverse)
  }

  case class BEdge(edge:UndEdge) extends Edge {
    val id = edge.id * -1 // not really necessary
    val from = edge.to
    val to   = edge.from
    val capacity = edge.flow

    assert (capacity > 0)

    val flow = 0
    val isUndirected = false
    def reverse = copy(edge.reverse)
    lazy val asDirected = List(this)
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