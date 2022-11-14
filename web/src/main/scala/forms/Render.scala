package forms

import com.raquo.laminar.api.L._
import forms.Field._
import forms.Style.BooleanStyle._

object Render {
  sealed trait FieldState[+A]
  object FieldState {

    /** Field has been created but there has not been any user input */
    case object Initialized extends FieldState[Nothing]

    /** Field has invalid user input */
    final case class Invalid(reason: String) extends FieldState[Nothing]

    /** Field has valid user input */
    final case class Valid[A](value: A) extends FieldState[A]

    def initialized[A]: FieldState[A] = Initialized
    def invalid[A](reason: String): FieldState[A] = Invalid(reason)
    def valid[A](value: A): FieldState[A] = Valid(value)

    def fromEither[A](either: Either[String, A]): FieldState[A] =
      either match {
        case Left(reason) => Invalid(reason)
        case Right(value) => Valid(value)
      }
  }

  def render[A](form: Form[A]): HtmlElement = {
    val (fieldsHtml, fieldsSignal) = renderField(form.fields)
    val submitted = new EventBus[Unit]
    val onSubmitted = (a: A) => println(a)
    val html =
      div(
        h1(className := "text-4xl font-bold mb-4", form.title),
        fieldsHtml,
        button(
          className := "ring-2 ring-sky-500 rounded-md border-2 border-sky-500 p-2 m-2 text-sky-500",
          typ := "submit",
          onClick.mapTo(()) --> submitted.writer,
          "Submit"
        ),
        submitted.events.sample(fieldsSignal) --> onSubmitted
      )

    html
  }

  def renderField[A](field: Field[A]): (HtmlElement, Signal[A]) = {
    def renderLabel(lbl: Option[String]): HtmlElement =
      div(className := "p-2", label(lbl))
    def renderInput(input: HtmlElement): HtmlElement =
      div(className := "col-span-3", input)
    def renderValidationMessage(
        invalid: Signal[Option[String]]
    ): HtmlElement =
      div(
        className <-- invalid.map(msg =>
          if (msg.isDefined) "visible" else "invisible"
        ),
        invalid.map(msg =>
          msg match {
            case None        => ""
            case Some(value) => value
          }
        )
      )
    def renderLabelAndInput(
        label: Option[String],
        input: HtmlElement,
        invalid: Signal[Option[String]]
    ): HtmlElement =
      div(
        className := "grid grid-cols-4 gap-2 p-2",
        renderLabel(label),
        renderInput(input),
        renderValidationMessage(invalid)
      )

    field match {
      case TextField(lbl, iV, ph, validation) =>
        val state = Var(FieldState.initialized[String])
        val output = Var(iV.getOrElse(""))
        val formInput =
          div(
            className := "col-span-3",
            input(
              typ := "text",
              className := "ring-2 ring-sky-500 rounded-md border-2 border-sky-500 p-2",
              // initial value takes precedence over placeholder
              iV.map(iV => value := iV)
                .orElse(ph.map(ph => placeholder := ph)),
              onInput.mapToValue --> output,
              onInput.mapToValue.map(v =>
                FieldState.fromEither(validation(v))
              ) --> state
            )
          )
        val html =
          renderLabelAndInput(
            lbl,
            formInput,
            state.signal.map(s =>
              s match {
                case FieldState.Initialized  => None
                case FieldState.Valid(_)     => None
                case FieldState.Invalid(msg) => Some(msg)
              }
            )
          )
        (html, output.signal)

      case BooleanField(style, lbl, iV) =>
        val output = Var(iV.getOrElse(false))
        val html =
          style match {
            case Checkbox =>
              renderLabelAndInput(
                lbl,
                div(
                  className := "col-span-3",
                  input(
                    className := "mr-2 my-2 border-2 border-sky-500",
                    typ := "checkbox"
                  )
                )
              )
            case Choice(trueChoice @ _, falseChoice @ _) =>
              renderLabelAndInput(
                lbl,
                div(
                  className := "col-span-3",
                  input(typ := "checkbox")
                )
              )
          }

        (html, output.signal)

      case Product(left, right) =>
        val (leftHtml, leftOutput) = renderField(left)
        val (rightHtml, rightOutput) = renderField(right)
        val html = div(leftHtml, rightHtml)
        val output = leftOutput.combineWith(rightOutput)

        (html, output)
    }
  }

}
