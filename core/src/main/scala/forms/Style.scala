package forms

object Style {
  sealed trait BooleanStyle
  object BooleanStyle {
    case object Checkbox extends BooleanStyle
    final case class Choice(trueChoice: String, falseChoice: String)
        extends BooleanStyle
  }

  object boolean {
    val checkbox: BooleanStyle = BooleanStyle.Checkbox
    def choice(trueChoice: String, falseChoice: String): BooleanStyle =
      BooleanStyle.Choice(trueChoice, falseChoice)
  }
}
