import io.Source
import java.io.PrintWriter
import collection.immutable.Queue

object Main {

  import NetworkFlow._

  // a simple network for testing the algorithm
  def testNetwork = {
    val nodes = "s,a,b,c,d,e,f,g,t".split(",")
        .zipWithIndex.map(t => FlowNode(t._2, t._1))
        .toIndexedSeq

    val edges = List(
      (0,1,12),(0,2,15),(0,3,20),(1,5,5),(1,6,2),(1,2,5),(2,3,11),(2,4,3),(2,6,6),
      (3,4,4),(3,7,8), (4,6,6),(4,7,1),(5,8,18),(5,6,9),(6,7,7),(6,8,13),(7,8,10)
    )

    RailNetwork.fromNodesAndEdges(nodes, edges)
  }


  def main(args:Array[String]):Unit = {

    if (args.length == 1 && args(0) == "test") {
      val Some(network) = parseNetworkFile("./data/rail.txt")
      runOnNetwork(network)
    } else if (args.length == 1) {
      parseNetworkFile(args(0)).map(runOnNetwork(_))
        .getOrElse({println("Network not parsed successfully")})
    } else {
      println("Reading from stdin")
      println("Press Ctrl+D or Ctrl+Z to quit")
      println("Call this program with a filepath to read from file instead")
      parseNetwork(Source.stdin.getLines).map(runOnNetwork(_))
        .getOrElse({println("Network from stdin not parsed successfully")})
    }
  }

  def runOnNetwork(network:RailNetwork):Unit = {
    val maxFlowNw = maxFlow(network, 100)
    val maxFlowNwRes = maxFlowNw.toResidual
    println("max flow: " + maxFlowNw.flowValue)
    val mincut = maxFlowNw.minSTCut
    val edges = mincut.edgesOnCut
    println("min cut:")
    println(
      edges
        .map({case (a,b) => (a,b, maxFlowNw.flowFrom(a,b))})
        .filter({case (a,b,c) => c >= 0})
        .sortWith(edgeCompare(_,_))
        .map({case (a,b,c) => s"$a $b $c"})
        .mkString("\n")
    )
  }

  private def edgeCompare(x:(Int,Int,Int), y:(Int,Int,Int)):Boolean =
    if (x._1 == y._1)
      x._2 < y._2
    else x._1 < y._1

  def parseNetwork(iter:Iterator[String]):Option[RailNetwork] = {

    import FunIterator._
    val fiter:FunIterator[String,RailNetwork] = for {
      nNodes <- head
      nodes <- take(nNodes.toInt)
      nEdges <- head
      edges <- take(nEdges.toInt)
      done <- isEmpty
    } yield {
      assert (done == true)
      val nodesSeq = nodes.zipWithIndex.map({case (lbl,i) => FlowNode(i, lbl)}).toIndexedSeq
      val edgesSeq = edges.map(_.split(" ").map(_.toInt))
        .map({case Array(from, to, c) => (from, to, c)})
      RailNetwork.fromNodesAndEdges(nodesSeq, edgesSeq)
    }
    val result = fiter.run(iter.toList)
    result.map(_._1)
  }

  def parseNetworkFile(filepath:String):Option[RailNetwork] = {

    val src = Source.fromFile(filepath)
    try {
      parseNetwork(src.getLines)
    } finally {
      src.close()
    }
  }

}