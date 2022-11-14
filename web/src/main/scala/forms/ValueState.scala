package forms

/** Stores the state of the value of a field:
  *   - uncommitted: the user has not yet committed a value, though they may be
  *     in the process of entering a value.
  *   - committed: the user has committed a value that can now be validated
  */
sealed trait ValueState[+A] {
  import ValueState._

  def isUncommitted: Boolean =
    this match {
      case Uncommitted => true
      case _           => false
    }
}
object ValueState {
  case object Uncommitted extends ValueState[Nothing]
  final case class Committed[A](value: A) extends ValueState[A]

  def uncommitted[A]: ValueState[A] = Uncommitted
  def committed[A](value: A): ValueState[A] = Committed(value)
}
