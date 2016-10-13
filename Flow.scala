
case class Node(id:Int, label:String)

trait Edge extends Graph.Edge {

  val capacity:Int
  def hasFiniteCapacity:Boolean = capacity > 0

}

// Undirected Edge
case class UndEdge(id:Int, from:Int, to:Int, capacity:Int) extends Edge {
  lazy val reverse = DirEdge(id, from = to, to = from, capacity = capacity)
  lazy val toDirected = DirEdge(id,from,to,capacity)
}

// Directed edge
case class DirEdge(id:Int, from:Int, to:Int, capacity:Int) extends Edge {
  lazy val reverse = copy(from = to, to = from)
}

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

object Flow {

}

case class Flow(map:Map[(Int,Int), Int] = Map()) {

  def value(ida:Int, idb:Int):Int = {
    map.get((ida,idb)).getOrElse(0)
  }

  def value(e:Edge):Int = value(e.from, e.to)

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


object FlowNetwork {

  @annotation.tailrec
  def maxFlow(nw:Network, c:Int = 1000):Network = {
    if (c <= 0) {
      nw
    } else {
      nw.assertValid()
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

}

trait FlowNetwork extends Graph[Node, Edge] {
  lazy val source = nodes.head
  lazy val sink = nodes.last
  lazy val internal = nodes.tail.inits
  lazy val sourceId = source.id
  lazy val sinkId = sink.id

  val flow:Flow

  def path2string(path:Path, nw:FlowNetwork):String = {
    val getFlow = (a:Int,b:Int) => nw.flow.value(a,b)
    nw.node(path.last.from).label +
      path.map(e => s" =${getFlow(e.from, e.to)}/${e.capacity}=> " + nw.node(e.to).label).reverse.mkString
  }

  def bottleneck(path:Path):Int = {
    path.filter(_.hasFiniteCapacity).map(e => e.capacity).min
  }


}

case class Network(nodes:IndexedSeq[Node], edges:Seq[UndEdge], flow:Flow = Flow())
           extends UndGraph[Node,UndEdge] with FlowNetwork {

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

  def toGraphViz:String = {
    def e2s(e:Edge):String = e match {
      case UndEdge(id, from, to, cap) => {
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

    s"""graph Network {
      rankdir=LR
      ${nodesStr}
      ${edgesStr}}
    """
  }

  def minSTCut:STCut = {
    val reachable = this.toResidual.reachableFrom(this.sourceId)
    STCut(reachable, this.nodes.diff(reachable), this.toResidual)
  }

  def assertValid():Unit = {
    // println("Asserting for " + this.getClass)
    edges.foreach(e => if (e.hasFiniteCapacity) assert(flow.value(e) <= e.capacity))
  }

  assertValid()

}

case class Residual(nw:Network) extends FlowNetwork with Graph[Node,Edge] {

  import Flow._

  lazy val edges = nw.edges.flatMap(e => {
    val cap = e.capacity
    val cap2 = if (cap < 0) (Integer.MAX_VALUE) else cap
    val e1 = DirEdge(0, e.from, e.to, cap2 - nw.flow(e.from, e.to))
    val e2 = DirEdge(0, e.to, e.from, cap2 - nw.flow(e.to, e.from))
    List(e1,e2).filter(_.capacity > 0)
  })

  val nodes = nw.nodes
  val flow = nw.flow

  def augment(path:Path, nw:Network):Network = {
    val b = bottleneck(path)
    // println("augment: " + path2string(path, nw) + " with " + b)
    path.foldLeft (nw) ((acc, e) => e match {
      case DirEdge(id, from, to, cap) => acc.increaseFlow(from, to, b)
      case _ => throw new RuntimeException("Only directed edges on path allowed")
    })
  }

  def assertValid() {
    // for (u <- r.nodes/*; v <- nodes if u.id != v.id && hasEdge(u.id,v.id)*/) {

    //   val flowOut = r.flow.flowOut(u.id)
    //   val flowIn  = r.flow.flowIn(u.id)

    //   // val flowIn  = edgesTo(u.id).map(e => flow(e.from, e.to))
    //   println(s"${u.id} in: $flowIn, out: $flowOut")
    // }
  }

  def toGraphViz:String = {
    def e2s(e:Edge):String = e match {
      case e@DirEdge(id, from, to, cap) => {
        s"""$from -> $to [label="$cap", len=3]"""
      }
    }
    val nodesStr = nodes.map(n => n.id + " [label=\"" + n.label + "\"]").mkString("\n")
    val edgesStr = edges
      .map(e2s(_))
      .mkString("\n")

    s"""digraph Network {
      rankdir=LR
      ${nodesStr}
      ${edgesStr}}
    """
  }
}