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
    println("max flow: " + maxFlowNw.flow.value)
    val mincut = maxFlowNw.minSTCut
    val edges = mincut.edgesOnCut
    println("min cut:")
    println(
      edges.sortBy(_.to)
        .map(e => s"${e.to} ${e.from} ${e.capacity}")
        .mkString("\n")
    )

    // print graphviz files!
    new PrintWriter("graph.dot") {
      write(maxFlowNw.toGraphViz); close
    }
    new PrintWriter("residual.dot") {
      write(maxFlowNw.toResidual.toGraphViz); close
    }

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