package controllers

import java.net.{ConnectException, URL}

import play.api.Play._
import play.api.Logger
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.{JsString, Json}
import play.api.mvc._
import play.api.libs.ws._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent.Future

trait WunderAPI extends Results {
  val baseURLString = "https://a.wunderlist.com/api/v1/"
  private def methodURL(wunderAPIMethod: Method) = {
    val base = if (baseURLString.last != '/') baseURLString + '/'
               else baseURLString
    new URL(new URL(base), wunderAPIMethod.spec)
  }

  protected  def authFail: Result

  trait Method {
    def spec: String
  }

  object Methods {
    type ListID = Long
    case object Lists extends Method {
      def spec = "./lists"
    }
    case class List(id: ListID) extends Method {
      def spec = s"${Lists.spec}/${id.toString}"
    }
    case class Tasks(listId: ListID) extends Method {
      def spec = s"./tasks?list_id=${listId.toString}"
    }
    case class Reminders(listId: ListID) extends Method {
      def spec = s"./reminders?list_id=${listId.toString}"
    }
  }

  case object UnauthorizedException extends Exception

  object WunderAction {
    type Response = (WSResponseHeaders, Enumerator[Array[Byte]])
    private def call(authToken: String)(m: Method): Future[Response] =
      WS.url(methodURL(m).toString)
        .withHeaders(
          "X-Access-Token" -> authToken,
          "X-Client-ID" -> Auth.clientId
        ).getStream().map {
        case resp@(h, body) if h.status == 200 => resp
        case resp@(h, _)    if h.status == 401 =>
          throw UnauthorizedException
      }

    def apply(f: (Method => Future[Response]) => Future[Result]) =
      Action.async { implicit req =>
        Auth.token match {
          case Some(authToken) =>
            f(call(authToken)) recover {
              case UnauthorizedException => authFail
              case e:ConnectException =>
                Logger.error("Wunderlist API is unreachable", e)
                ServiceUnavailable
              case e =>
                Logger.error("Wunderlist API error", e)
                InternalServerError
            }
          case None =>
            Future { authFail }
        }
      }
  }
}

object Application extends Controller with WunderAPI {

  def authFail = Redirect(routes.Auth.auth()).withNewSession

  def lists = WunderAction { api =>
    api(Methods.Lists) map {
      case (_, body) =>
        Ok.chunked(body) as JSON
    }
  }

  def tasks(listId: Long) = WunderAction { api =>
    api(Methods.Tasks(listId)) map {
      case (_, body)=>
        Ok.chunked(body) as JSON
    }
  }

}
