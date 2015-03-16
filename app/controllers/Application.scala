package controllers

import play.api.Play._
import play.api.libs.json.Json
import play.api.mvc._
import play.api.libs.ws._
import views.html.helper

import scala.util.Random

object Application extends Controller {
  val ID_CONF="wundercal.client.id"
  val SECRET_CONF="wundercal.client.secret"
  val WUNDERBASE="https://www.wunderlist.com/oauth/authorize"
  val client_id = application.configuration.getString(ID_CONF).get

  def index = Action { implicit req =>
    val encCB = helper.urlEncode(routes.Application.wunderback().absoluteURL(false))
    val state = helper.urlEncode(Random.nextInt().toString)
    Redirect(s"$WUNDERBASE?client_id=$client_id&redirect_uri=$encCB&state=$state")
  }

  def wunderback(code: String, state: String) = Action {
    Ok(Json.obj("code" -> code, "state" -> state)) as JSON
  }
}
