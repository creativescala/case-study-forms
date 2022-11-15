package forms

import com.raquo.laminar.api.L._
import org.scalajs.dom
import forms.ValidationState._

object Main {
  val appContainer: dom.Element = dom.document.querySelector("#appContainer")
  val weather = Field.text
    .withLabel("What is the weather like?")
    .withValidation(answer =>
      answer.toLowerCase() match {
        case "rainy" => Right(answer)
        case "rain"  => Right(answer)
        case _       => Left(s"$answer? More like rain I think.")
      }
    )
  val awesome = Field.boolean.withLabel("Is everything awesome?")
  val agree = Field.boolean
    .withLabel("Do you agree that pizza is a type of toast?")
    .withStyle(Style.boolean.choice("Yes", "No"))

  val question = weather.product(awesome).product(agree)
  val form = Form("The Awesome Form", question)

  val appElement: HtmlElement = {
    val (html, output) = Render.render(form)
    div(
      html,
      h2(className := "text-xl font-bold mb-2", "Submission"),
      div(
        child <-- output.map(vs =>
          vs match {
            case Invalid(_) => p("Come on, enter some valid input.")
            case Unchecked =>
              p("You need to enter input. You can't just leave it blank!")
            case Valid(((weather, awesome), pizza)) =>
              div(
                p("You said:"),
                ol(
                  className := "list-disc m-2",
                  li(s"The weather is $weather"),
                  li(s"The awesomeness is set to ${awesome.toString()}"),
                  li(
                    s"You ${if (pizza) "do" else "do not"} agree that pizza is a type of toast."
                  )
                )
              )
          }
        )
      )
    )
  }

  def main(args: Array[String]): Unit =
    renderOnDomContentLoaded(appContainer, appElement)
}
