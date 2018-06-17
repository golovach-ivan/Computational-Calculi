package cc.dyck

object Dyck2 extends App {
  import cc.LangLib._

  sealed trait Dyck
  case object ε extends Dyck
  case object ≺ extends Dyck
  case object ≻ extends Dyck
  final case class App(d0: Dyck, d1: Dyck) extends Dyck

  def show(term: Dyck): String = term match {
    case `ε` => ""
    case `≺` => "("
    case `≻` => ")"
    case App(d0, d1) => s"(${show(d0)})${show(d1)}"
  }

  // D := ε|(D)D
  def D: Stream[Dyck] =
    ε #:: (App ⊲ (D, D))

  D take 100 foreach(t => println(show(t)))
}



