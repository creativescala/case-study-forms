package forms

import com.raquo.laminar.api.L._
import org.scalajs.dom

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
  val question = weather.product(awesome)
  val form = Form("The Awesome Form", question)

  val appElement: HtmlElement =
    Render.render(form)

  def main(args: Array[String]): Unit =
    renderOnDomContentLoaded(appContainer, appElement)
}
