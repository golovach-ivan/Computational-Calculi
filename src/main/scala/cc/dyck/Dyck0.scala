package cc.dyck

/**
  * Simplest case: NO lang classes.
  */
object Dyck0 extends App {
  import cc.StreamLib._

  // D := ε|(D)D
  def D: Stream[String] =
    "" #:: ((D ⨯ D) map { case (d0, d1) => s"($d0)$d1" })

  D take 100 foreach println
}



