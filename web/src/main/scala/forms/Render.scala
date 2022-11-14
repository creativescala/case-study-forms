package forms

import com.raquo.laminar.api.L._
import forms.Field._
import forms.Style.BooleanStyle._

object Render {
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
    def renderValidation(
        validationState: Signal[ValidationState]
    ): HtmlElement =
      p(
        className <-- validationState.map(v =>
          if (v.toOption.isDefined) "visible col-end-5 col-span-3 text-red-500"
          else "invisible p-2"
        ),
        child <-- validationState.map(v =>
          v.toOption match {
            case Some(message) => message
            case None          => ""
          }
        )
      )
    def renderLabelAndInput(
        label: Option[String],
        input: HtmlElement,
        validationState: Signal[ValidationState]
    ): HtmlElement =
      div(
        className := "grid grid-cols-4 gap-x-2 gap-y-1 p-2",
        renderLabel(label),
        renderInput(input),
        renderValidation(validationState)
      )

    field match {
      case TextField(label, iV, ph, validation) =>
        val validationState = Var(ValidationState.unchecked)
        val validate: Observer[String] =
          validationState.writer.contramap(str =>
            ValidationState.fromEither(validation(str))
          )
        val output = Var(iV.getOrElse(""))
        val html =
          renderLabelAndInput(
            label,
            div(
              className := "col-span-3",
              input(
                typ := "text",
                className := "ring-2 ring-sky-500 rounded-md border-2 border-sky-500 p-2",
                // initial value takes precedence over placeholder
                iV.map(iV => value := iV)
                  .orElse(ph.map(ph => placeholder := ph)),
                onInput.mapToValue --> output,
                onBlur.mapToValue --> validate,
                onChange.mapToValue --> validate
              )
            ),
            validationState.signal
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
                ),
                Signal.fromValue(ValidationState.valid)
              )
            case Choice(trueChoice @ _, falseChoice @ _) =>
              renderLabelAndInput(
                lbl,
                div(
                  className := "col-span-3",
                  input(typ := "checkbox")
                ),
                Signal.fromValue(ValidationState.valid)
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
