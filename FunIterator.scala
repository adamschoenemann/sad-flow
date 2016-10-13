
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