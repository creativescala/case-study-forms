package forms

/** A form field that produces a value of type A when submitted. */
sealed trait Field[A] {
  import Field._

  def product[B](that: Field[B]): Field[(A, B)] =
    Product(this, that)

  def zip[B](that: Field[B]): Field[(A, B)] =
    product(that)

}
object Field {
  final case class Product[A, B](left: Field[A], right: Field[B])
      extends Field[(A, B)]

  final case class TextField(
      label: Option[String],
      initialValue: Option[String],
      placeholder: Option[String],
      validation: String => Either[String, String]
  ) extends Field[String] {
    def withLabel(label: String): TextField =
      this.copy(label = Some(label))

    def withInitialValue(initialValue: String): TextField =
      this.copy(initialValue = Some(initialValue))

    def withPlaceholder(placeholder: String): TextField =
      this.copy(placeholder = Some(placeholder))

    def withValidation(
        validation: String => Either[String, String]
    ): TextField =
      this.copy(validation = validation)
  }

  final case class BooleanField(
      style: Style.BooleanStyle,
      label: Option[String],
      initialValue: Option[Boolean]
  ) extends Field[Boolean] {
    def withStyle(style: Style.BooleanStyle): BooleanField =
      this.copy(style = style)

    def withLabel(label: String): BooleanField =
      this.copy(label = Some(label))

    def withInitialValue(initialValue: Boolean): BooleanField =
      this.copy(initialValue = Some(initialValue))
  }

  /** Validation method that always returns the given value. I.e. is always
    * valid.
    */
  def always[A](value: A): Either[String, A] = Right(value)

  val text: TextField =
    TextField(None, None, None, always)

  val boolean: BooleanField =
    BooleanField(Style.boolean.checkbox, None, None)
}
