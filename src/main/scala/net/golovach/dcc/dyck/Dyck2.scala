package net.golovach.dcc.dyck

object Dyck2 extends App {
  import net.golovach.dcc.LangLib._

  sealed trait Dyck
  case object ε extends Dyck
  final case class App(d0: Dyck, d1: Dyck) extends Dyck

  def show(term: Dyck): String = term match {
    case `ε` => ""
    case App(d0, d1) => s"(${show(d0)})${show(d1)}"
  }

  // D := ε|(D)D
  def D: Stream[Dyck] =
    ε #:: (App ⊲ (D, D))

  D take 1000 foreach(t => println(show(t)))
}



