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
      placeholder: Option[String]
  ) extends Field[String] {
    def withLabel(label: String): TextField =
      this.copy(label = Some(label))

    def withInitialValue(initialValue: String): TextField =
      this.copy(initialValue = Some(initialValue))

    def withPlaceholder(placeholder: String): TextField =
      this.copy(placeholder = Some(placeholder))
  }

  final case class BooleanField(
      style: Style.BooleanStyle,
      label: Option[String],
      initialValue: Option[Boolean]
  ) extends Field[String] {
    def withStyle(style: Style.BooleanStyle): BooleanField =
      this.copy(style = style)

    def withLabel(label: String): BooleanField =
      this.copy(label = Some(label))

    def withInitialValue(initialValue: Boolean): BooleanField =
      this.copy(initialValue = Some(initialValue))
  }

  val text: TextField =
    TextField(None, None, None)

  val boolean: BooleanField =
    BooleanField(Style.boolean.checkbox, None, None)
}
