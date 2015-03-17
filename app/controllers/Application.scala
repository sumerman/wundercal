package controllers

import play.api.Play._
import play.api.libs.json.{JsString, Json}
import play.api.mvc._
import play.api.libs.ws._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent.Future

object Application extends Controller {

  def lists = Action.async { implicit req =>
    Auth.token match {
      case Some(authToken) =>
        WS.url("https://a.wunderlist.com/api/v1/lists")
          .withHeaders(
            "X-Access-Token" -> authToken,
            "X-Client-ID" -> Auth.clientId
          ).get map { resp =>
          Ok(resp.body) as JSON // Handle unauthorized
        }
      case None =>
        Future { Redirect(routes.Auth.auth()) }
    }
  }

}
