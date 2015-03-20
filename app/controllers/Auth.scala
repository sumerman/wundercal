package controllers

import play.api.Play._
import play.api.libs.json.{JsString, Json}
import play.api.mvc._
import play.api.libs.ws._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import views.html.helper
import scala.concurrent.Future
import scala.util.Random

trait AuthBase { self: Controller =>
  val ID_CONF="wundercal.client.id"
  val SECRET_CONF="wundercal.client.secret"
  val SECURE_CONF="wundercal.client.secure_back"

  val clientId = application.configuration.getString(ID_CONF).getOrElse("")
  val clientSecret = application.configuration.getString(SECRET_CONF).getOrElse("")
  val clientSecure = application.configuration.getBoolean(SECURE_CONF).getOrElse(false)

  val WUNDERAUTH="https://www.wunderlist.com/oauth/authorize"
  val WUNDERCODE="https://www.wunderlist.com/oauth/access_token"

  val sessionStateKey = "randstate"
  val sessionAuthKey = "wunderauth"

  def token[A](implicit req: Request[A]): Option[String] = req.session.get(sessionAuthKey)

  def wunderbackRoute: Call
  def successRoute: Call

  def auth = Action { implicit req =>
    val encCB = helper.urlEncode(wunderbackRoute.absoluteURL(clientSecure))
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
            Redirect(successRoute).withSession(sessionAuthKey -> authToken)
          case _ => BadGateway
        }
      }
    else Future {
      BadRequest("State in callback does not match the one that was sent.")
    }
  }

}

object Auth extends Controller with AuthBase {
  def wunderbackRoute = routes.Auth.wunderback()
  def successRoute = routes.Application.index()
}
