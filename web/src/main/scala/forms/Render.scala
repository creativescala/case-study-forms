package forms

import com.raquo.laminar.api.L._
import forms.Field._
import forms.Style.BooleanStyle._

object Render {
  def render[A](form: Form[A]): HtmlElement = {
    val (fieldsHtml, _) = renderField(form.fields)
    div(
      h1(className := "text-4xl font-bold mb-4", form.title),
      fieldsHtml
    )
  }

  def renderField[A](field: Field[A]): (HtmlElement, Signal[A]) = {
    def renderLabel(lbl: Option[String]): HtmlElement =
      div(className := "p-2", label(lbl))
    def renderInput(input: HtmlElement): HtmlElement =
      div(className := "col-span-3", input)
    def renderLabelAndInput(
        label: Option[String],
        input: HtmlElement
    ): HtmlElement =
      div(
        className := "grid grid-cols-4 gap-2",
        renderLabel(label),
        renderInput(input)
      )

    field match {
      case TextField(lbl, iV, ph) =>
        val output = Var(iV.getOrElse(""))
        val html =
          renderLabelAndInput(
            lbl,
            div(
              className := "col-span-3",
              input(
                typ := "text",
                className := "ring-2 ring-sky-500 rounded-md border-2 border-sky-500 p-2",
                // initial value takes precedence over placeholder
                iV.map(iV => value := iV)
                  .orElse(ph.map(ph => placeholder := ph)),
                onInput.mapToValue --> output
              )
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
