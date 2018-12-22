object Extensions {
  class SeqExtensions[T](list: Seq[T]) {
    def myLength2: Int = list.length

    def mapi(fn: (T, Int) => T): Seq[T] = {
      list.zipWithIndex.map { case (a, b) => fn(a, b) }
    }
  }

  implicit def seqExtensions[T](list: Seq[T]): SeqExtensions[T] = new SeqExtensions[T](list)
}
