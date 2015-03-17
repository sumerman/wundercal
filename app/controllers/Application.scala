package controllers

import play.api.Play._
import play.api.libs.json.{JsString, Json}
import play.api.mvc._
import play.api.libs.ws._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import views.html.helper
import scala.concurrent.Future
import scala.util.Random

object Application extends Controller {

  val ID_CONF="wundercal.client.id"
  val SECRET_CONF="wundercal.client.secret"
  val WUNDERAUTH="https://www.wunderlist.com/oauth/authorize" // TODO to config
  val WUNDERCODE="https://www.wunderlist.com/oauth/access_token" // TODO to config

  val clientId = application.configuration.getString(ID_CONF).getOrElse("")
  val clientSecret = application.configuration.getString(SECRET_CONF).getOrElse("")

  val sessionStateKey = "randstate"
  val sessionAuthKey = "wunderauth"

  def auth = Action { implicit req =>
    val encCB = helper.urlEncode(routes.Application.wunderback().absoluteURL(false))
    val state = helper.urlEncode(Random.nextInt().toString)
    Redirect(s"$WUNDERAUTH?client_id=$clientId&redirect_uri=$encCB&state=$state")
      .withSession(req.session + (sessionStateKey -> state))
  }

  def wunderback(code: String, state: String) = Action.async { implicit req =>
    val postData = Json.obj(
      "client_id" -> clientId,
      "client_secret" -> clientSecret,
      "code" -> code
    )
    val expectedState = req.session.get(sessionStateKey).getOrElse("")
    if (state == expectedState)
      WS.url(WUNDERCODE).post(postData) map { resp =>
        Json.parse(resp.body) \ "access_token" match {
          case JsString(authToken) =>
            Redirect(routes.Application.lists()).withSession(sessionAuthKey -> authToken)
          case _ => BadGateway
        }
      }
    else Future {
      BadRequest("State in callback does not match the one that was sent.")
    }
  }

  def lists = Action.async { implicit req =>
    req.session.get(sessionAuthKey) match {
      case Some(authToken) =>
        WS.url("https://a.wunderlist.com/api/v1/lists")
          .withHeaders(
            "X-Access-Token" -> authToken,
            "X-Client-ID" -> clientId
          ).get map { resp =>
          Ok(resp.body) as JSON
        }
      case None =>
        Future { Redirect(routes.Application.auth()) }
    }
  }
}
