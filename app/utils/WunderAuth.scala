package utils

import play.api.Play._
import play.api.libs.json.Json
import play.api.libs.ws.WS
import play.api.mvc.{Action, Call, Request, Controller}
import views.html.helper

import scala.concurrent.Future
import scala.util.Random

trait WunderAuth { self: Controller =>
  import play.api.libs.concurrent.Execution.Implicits.defaultContext

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
        (Json.parse(resp.body) \ "access_token").asOpt[String] match {
          case Some(authToken) =>
            Redirect(successRoute).withSession(sessionAuthKey -> authToken)
          case _ => BadGateway
        }
      }
    else Future {
      BadRequest("State in callback does not match the one that was sent.")
    }
  }
}

