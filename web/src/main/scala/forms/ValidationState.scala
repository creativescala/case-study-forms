package forms

import cats.data.NonEmptyChain

/** Stores state of validation on a field, or combination of fields:
  *   - unchecked: we have not run validation, usually because the user has not
  *     yet interacted with the field
  *   - valid: the valid values the user entered
  *   - invalid: the list of reasons the values are invalid
  */
sealed trait ValidationState[+A] {
  import ValidationState._

  /** True if the state is not Unchecked */
  def isChecked: Boolean =
    this match {
      case Valid(_)   => true
      case Invalid(_) => true
      case Unchecked  => false
    }

  def product[B](that: ValidationState[B]): ValidationState[(A, B)] =
    (this, that) match {
      case (Valid(r1), Valid(r2))     => Valid((r1, r2))
      case (Invalid(r1), Invalid(r2)) => Invalid(r1 ++ r2)
      case (Unchecked, Unchecked)     => Unchecked
      // Invalid takes preference over Valid and Unchecked
      case (Invalid(r), _) => Invalid(r)
      case (_, Invalid(r)) => Invalid(r)
      // Unchecked takes preference over Valid
      case (Unchecked, _) => Unchecked
      case (_, Unchecked) => Unchecked
    }

  def zip[B](that: ValidationState[B]): ValidationState[(A, B)] =
    product(that)

  def toReasons: Option[NonEmptyChain[String]] =
    this match {
      case Invalid(reason) => Some(reason)
      case Unchecked       => None
      case Valid(_)        => None
    }
}
object ValidationState {

  /** The validation status has not been checked as the user has not focused the
    * element
    */
  case object Unchecked extends ValidationState[Nothing]

  /** Validation has failed with the given reason */
  final case class Invalid(reason: NonEmptyChain[String])
      extends ValidationState[Nothing]

  /** Validation has succeeded with the given value */
  final case class Valid[A](result: A) extends ValidationState[A]

  def unchecked[A]: ValidationState[A] = Unchecked
  def invalid[A](reason: String): ValidationState[A] =
    Invalid(
      NonEmptyChain.one(reason)
    )
  def valid[A](result: A): ValidationState[A] = Valid(result)

  def fromEither[A](either: Either[String, A]): ValidationState[A] =
    either match {
      case Left(reason)  => invalid(reason)
      case Right(result) => valid(result)
    }
}
