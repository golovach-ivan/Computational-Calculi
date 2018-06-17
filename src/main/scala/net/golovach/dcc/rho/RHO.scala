package net.golovach.dcc.rho

object RHO extends App {

  import net.golovach.dcc.LangLib._
  import net.golovach.dcc.StreamLib._

  sealed trait Process
  case object ∅ extends Process
  final case class Par(p: Process, q: Process) extends Process
  final case class Lift(x: Channel, p: Process) extends Process
  final case class Input(c0: Channel, c1: Channel, p: Process) extends Process
  final case class Drop(c: Channel) extends Process
  sealed trait Channel
  final case class Quote(p: Process) extends Channel

  /**
    * "External" toString method.
    * @param pc only Process or Channel allowed.
    */
  def show(pc: Any): String = pc match {
    case `∅` => "∅"
    case Par(p, q) => s"(${show(p)}|${show(q)})"
    case Lift(c: Channel, p: Process) => s"${show(c)}!(${show(p)})"
    case Input(c0: Channel, c1: Channel, p: Process) => s"(${show(c0)})?(${show(c1)}).(${show(p)})"
    case Drop(c: Channel) => s"*(${show(c)})"
    case Quote(p: Process) => p match {
      case `∅` => "@∅"
      case _ => s"@(${show(p)})"
    }
  }

  def P: Stream[Process] =
    ∅ #:: ((Par ⊲ (P, P)) ⊕ (Lift ⊲ (C, P)) ⊕ (Input ⊲ (C, C, P)) ⊕ (Drop ⊲ C))

  def C: Stream[Channel] =
    Quote ⊲ P

  P take 100 foreach(p => println(show(p)))
}



