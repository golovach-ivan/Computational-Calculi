package net.golovach.dcc.dyck

import scala.collection.mutable

object Dyck1 extends App {
  import net.golovach.dcc.StreamLib._

  sealed trait Dyck
  case object ε extends Dyck
  case object ≺ extends Dyck
  case object ≻ extends Dyck
  case object ? extends Dyck
  final case class TermSeq(ds: Seq[Dyck]) extends Dyck

  def fill(termWithHoles: Seq[Dyck], values: Seq[Dyck]): TermSeq = {
    val termIter: Iterator[Dyck] = termWithHoles.iterator
    val valIter: Iterator[Dyck] = values.iterator

    val result = mutable.ArrayBuffer.empty[Dyck]
    while (termIter.hasNext) {
      val t = termIter.next()
      result += (if (t != ?) t else valIter.next)
    }
    TermSeq(result)
  }

  def prod(sx: Seq[Stream[Dyck]]): Stream[Seq[Dyck]] = sx match {
    case Seq(a, b) => (a ⨯ b).map({case (x, y) => Seq(x, y)})
  }

  implicit class Ops(ds: Seq[Dyck]) {
    def ⊲(tx: Seq[Stream[Dyck]]): Stream[Dyck] =
      prod(tx) map (fill(ds, _))
  }

  def show(term: Dyck): String = term match {
    case `ε` => ""
    case `≺` => "("
    case `≻` => ")"
    case TermSeq(ds: Seq[Dyck]) => ds.map(x => show(x)).mkString("")
  }

  // D := ε|(D)D
  def D: Stream[Dyck] =
    ε #:: (Seq(≺, ?, ≻, ?) ⊲ Seq(D, D))

  D take 1000 foreach(t => println(show(t)))
}



