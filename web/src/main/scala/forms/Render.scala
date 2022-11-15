package forms

import com.raquo.laminar.api.L._
import scala.util.Random
import forms.Field._
import forms.Style.BooleanStyle._

object Render {

  /** Generate a random alphanumeric string */
  def randomAlphanumeric(): String = Random.alphanumeric.take(8).mkString

  def render[A](
      form: Form[A]
  ): (HtmlElement, EventStream[ValidationState[A]]) = {
    val (fieldsHtml, fieldsSignal) = renderField(form.fields)
    val submitted = new EventBus[Unit]
    val output = new EventBus[ValidationState[A]]
    val html =
      div(
        className := "mb-4",
        h1(className := "text-4xl font-bold mb-4", form.title),
        fieldsHtml,
        button(
          className := "ring-2 ring-sky-500 rounded-md border-2 border-sky-500 p-2 text-sky-500",
          typ := "submit",
          onClick.mapTo(()) --> submitted.writer,
          "Submit"
        ),
        submitted.events.sample(fieldsSignal) --> output
      )

    (html, output.events)
  }

  def renderField[A](
      field: Field[A]
  ): (HtmlElement, Signal[ValidationState[A]]) = {
    def renderLabel(lbl: Option[String]): HtmlElement =
      div(label(lbl))
    def renderInput(input: HtmlElement): HtmlElement =
      div(className := "col-span-3", input)
    def renderValidation(
        validationState: Signal[ValidationState[A]]
    ): HtmlElement =
      p(
        className <-- validationState.map(v =>
          if (v.toReasons.isDefined) "visible col-end-5 col-span-3 text-red-500"
          else "invisible p-2"
        ),
        child <-- validationState.map(v =>
          v.toReasons match {
            case Some(messages) => messages.toChain.toList.mkString(",")
            case None           => ""
          }
        )
      )
    def renderLabelAndInput(
        label: Option[String],
        input: HtmlElement,
        validationState: Signal[ValidationState[A]]
    ): HtmlElement =
      div(
        className := "grid grid-cols-4 gap-x-2 gap-y-1",
        renderLabel(label),
        renderInput(input),
        renderValidation(validationState)
      )

    field match {
      case TextField(label, iV, ph, validation) =>
        val state = Var(
          iV.fold(ValueState.uncommitted[String])(v => ValueState.committed(v))
        )
        val validationState = state.signal.map(state =>
          state match {
            case ValueState.Uncommitted => ValidationState.unchecked[String]
            case ValueState.Committed(v) =>
              ValidationState.fromEither(validation(v))
          }
        )
        val commitValue =
          state.writer.contramap[String](v => ValueState.committed(v))

        val html =
          renderLabelAndInput(
            label,
            input(
              typ := "text",
              className := "ring-2 ring-sky-500 rounded-md border-2 border-sky-500 p-2",
              // initial value takes precedence over placeholder
              iV.map(iV => value := iV)
                .orElse(ph.map(ph => placeholder := ph)),
              onInput.mapToValue --> state.writer.contramapOpt[String](v =>
                if (state.now().isUncommitted) None
                else Some(ValueState.committed(v))
              ),
              onBlur.mapToValue --> commitValue,
              onChange.mapToValue --> commitValue
            ),
            validationState.signal
          )
        (html, validationState)

      case BooleanField(style, lbl, iV) =>
        val state = Var(
          iV.fold(ValueState.uncommitted[Boolean])(v => ValueState.committed(v))
        )
        val validationState = state.signal.map(state =>
          state match {
            case ValueState.Uncommitted  => ValidationState.unchecked[Boolean]
            case ValueState.Committed(v) => ValidationState.valid(v)
          }
        )
        val commitValue =
          state.writer.contramap[Boolean](v => ValueState.committed(v))

        val html =
          style match {
            case Checkbox =>
              renderLabelAndInput(
                lbl,
                input(
                  className := "mr-2 my-2 border-2 border-sky-500",
                  typ := "checkbox",
                  onClick.mapToChecked --> commitValue
                ),
                validationState
              )
            case Choice(trueChoice, falseChoice) =>
              val n = randomAlphanumeric()
              val trueId = randomAlphanumeric()
              val falseId = randomAlphanumeric()
              renderLabelAndInput(
                lbl,
                fieldSet(
                  label(className := "pr-1", forId := trueId, trueChoice),
                  input(
                    typ := "radio",
                    idAttr := trueId,
                    name := n,
                    onChange.mapTo(true) --> commitValue
                  ),
                  label(
                    className := "pl-2 pr-1",
                    forId := falseId,
                    falseChoice
                  ),
                  input(
                    typ := "radio",
                    idAttr := falseId,
                    name := n,
                    onChange.mapTo(false) --> commitValue
                  )
                ),
                validationState
              )
          }

        (html, validationState)

      case Product(left, right) =>
        val (leftHtml, leftOutput) = renderField(left)
        val (rightHtml, rightOutput) = renderField(right)
        val html = div(leftHtml, rightHtml)
        val output =
          leftOutput.combineWith(rightOutput).map { case (l, r) => l.zip(r) }

        (html, output)
    }
  }

}
