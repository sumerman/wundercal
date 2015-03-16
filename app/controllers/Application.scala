package controllers
import play.api.mvc._
import play.api.libs.ws._

object Application extends Controller {
  def index = Action {
    Ok(
      """
        |Hello World!
      """.stripMargin).as("text/plain")
  }
}
