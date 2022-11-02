package forms

final case class Form[A](title: String, fields: Field[A])
