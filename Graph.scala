
import collection.immutable.Queue



object Graph {

  trait Node {
    val id:Int
    val label:String
  }

  trait Edge {
    val id:Int
    val from:Int
    val to:Int
    val reverse:Edge
  }

  trait DirEdge extends Edge {

  }

  trait UndEdge extends Edge {
    val toDirected:DirEdge
    lazy val asDirected = List(toDirected, toDirected.reverse)
  }
}

trait Graph[+N <: Node, +E <: Edge] {

  type Path = List[E]

  val nodes:Seq[N]
  val edges:Seq[E]

  def node(id:Int):N = nodes(id)
  def edge(index:Int):E = edges(index)
  def edgesFrom(nodeId:Int):Seq[E] = edges.filter(_.from == nodeId)
  def edgesTo(nodeId:Int):Seq[E] = edges.filter(_.to == nodeId)

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
    def helper(queue:Queue[Path], explored:Set[Int], goal:N):Option[Path] = {
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

trait UndGraph[+N <: Node, +E <: UndEdge] extends Graph[N, E] {

  override def edgesFrom(nid:Int):Seq[E] = super.edgesFrom(nid) ++
    super.edgesTo(nid).map(_.reverse.asInstanceOf[E])

  override def edgesTo(nid:Int):Seq[E] = super.edgesTo(nid) ++
    super.edgesFrom(nid).map(_.reverse.asInstanceOf[E])
}

