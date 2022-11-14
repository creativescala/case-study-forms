package forms

sealed trait ValidationState {
  import ValidationState._

  def toOption: Option[String] =
    this match {
      case Invalid(reason) => Some(reason)
      case Unchecked       => None
      case Valid           => None
    }
}
object ValidationState {

  /** The validation status has not been checked as the user has not focused the
    * element
    */
  case object Unchecked extends ValidationState

  /** Validation has failed with the given reason */
  final case class Invalid(reason: String) extends ValidationState

  /** Validation has succeeded */
  case object Valid extends ValidationState

  val unchecked: ValidationState = Unchecked
  def invalid(reason: String): ValidationState = Invalid(reason)
  val valid: ValidationState = Valid

  def fromEither[A](either: Either[String, A]): ValidationState =
    either match {
      case Left(reason) => Invalid(reason)
      case Right(_)     => Valid
    }
}
