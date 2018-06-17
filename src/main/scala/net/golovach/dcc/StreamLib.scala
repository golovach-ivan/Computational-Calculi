package net.golovach.dcc

object StreamLib {

  /**
    * Pimp-my-library pattern. <br/>
    * Implicitly add binary combinators ⨯ and ⊕ to Stream class.
    */
  implicit class streamOps[A](a: => Stream[A]) {
    /** Stream cartesian product. <br/>
      * a⨯b = (a[0],b[0]) #:: (a[1],b[0]) #:: (a[0],b[1]) #:: (a[1],b[1]) #:: (a[0],b[2]) ...
      */
    def ⨯[B](b: => Stream[B]): Stream[(A, B)] = {
      lazy val col = a.tail.map((_, b.head))
      lazy val row = b.tail.map((a.head, _))
      lazy val square = a.tail ⨯ b.tail
      (a.head, b.head) #:: (col ⊕ row ⊕ square)
    }

    /**
      * Stream merge. <br/>
      * a⊕b = a[0] #:: b[0] #:: a[1] #:: b[1] #:: a[2] #:: ... <br/>
      * Result type C is the least upper bound of A and B.
      * */
    def ⊕[B <: C, C >: A](b: => Stream[B]): Stream[C] =
      (a.head: C) #:: (b ⊕ (a.tail: Stream[C]))
  }
}
