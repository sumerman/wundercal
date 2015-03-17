package controllers

import play.api.Play._
import play.api.libs.json.{JsString, Json}
import play.api.mvc._
import play.api.libs.ws._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent.Future

trait WunderAPI {
  def authFail: Result

  object WunderAction {
    def apply(okf: WSResponse => Result) =
      Action.async { implicit req =>
        Auth.token match {
          case Some(authToken) =>
            WS.url("https://a.wunderlist.com/api/v1/lists")
              .withHeaders(
                "X-Access-Token" -> authToken,
                "X-Client-ID" -> Auth.clientId
              ).get().map
            {
              resp =>
                if (resp.status == 200) okf(resp)
                else authFail
            }
          case None =>
            Future { authFail }
        }
      }
  }
}

object Application extends Controller with WunderAPI {

  def authFail = Redirect(routes.Auth.auth()).withNewSession

  def lists = WunderAction { resp =>
    Ok(resp.body) as JSON
  }

}
