package cc

/**
  * Pimp-my-library pattern. <br/>
  * Implicitly add operator ⊲ to case classes. <br/>
  * Convert case class to super-stream-zipper.
  * Example: {{{
  *   case class Person(name: String, age: Int)
  *   val names: Stream[String] = ???
  *   val ages: Stream[Int] = ???
  *
  *   val people: Stream[Person] = Person ⊲ (names, ages)
  * }}}
  */
object LangLib {
  import StreamLib._

  implicit class Ops1[A0, B](f: A0 => B) {
    def ⊲(p: Stream[A0]): Stream[B] = p map f
  }

  implicit class Ops2[A0, A1, B](f: (A0, A1) => B) {
    def ⊲(tx: (Stream[A0], Stream[A1])): Stream[B] =
      (tx._1 ⨯ tx._2).map ({case (p, q) => f(p, q)})
  }

  implicit class Ops3[A0, A1, A2, B](f: (A0, A1, A2) => B) {
    def ⊲(tx: (Stream[A0], Stream[A1], Stream[A2])): Stream[B] =
      (tx._1 ⨯ tx._2 ⨯ tx._3).map ({case ((p, q), r) => f(p, q, r)})
  }

  implicit class Ops4[A0, A1, A2, A3, B](f: (A0, A1, A2, A3) => B) {
    def ⊲(tx: (Stream[A0], Stream[A1], Stream[A2], Stream[A3])): Stream[B] =
      (tx._1 ⨯ tx._2 ⨯ tx._3 ⨯ tx._4).map ({case (((p, q), r), s) => f(p, q, r, s)})
  }

  implicit class Ops5[A0, A1, A2, A3, A4, B](f: (A0, A1, A2, A3, A4) => B) {
    def ⊲(tx: (Stream[A0], Stream[A1], Stream[A2], Stream[A3], Stream[A4])): Stream[B] =
      (tx._1 ⨯ tx._2 ⨯ tx._3 ⨯ tx._4 ⨯ tx._5).map ({case ((((p, q), r), s), u) => f(p, q, r, s, u)})
  }
}
