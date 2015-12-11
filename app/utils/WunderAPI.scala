package utils

import java.net.{ConnectException, URL}

import controllers.Auth
import play.api.Logger
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.JsValue
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

trait WunderAPI {
  import play.api.Play.current
  protected def authFail: Result

  trait Method {
    val baseURLString = "https://a.wunderlist.com/api/v1/"
    protected def methodURL(path: String) = {
      val base = if (baseURLString.last != '/') baseURLString + '/' else baseURLString
      new URL(new URL(base), path)
    }

    val path: String
    val url = methodURL(path)
  }

  object Methods {
    type ListID = Long
    case object Folders extends { val path = s"./folders" } with Method
    case object Lists extends { val path = "./lists" } with Method
    case class List(id: ListID) extends {
      val path = s"${Lists.path}/${id.toString}"
    } with Method
    case class Tasks(listId: ListID) extends {
      val path = s"./tasks?list_id=${listId.toString}"
    } with Method
    case class Reminders(listId: ListID) extends {
      val path = s"./reminders?list_id=${listId.toString}"
    } with Method
  }

  import play.api.libs.ws._

  case object UnauthorizedException      extends Exception
  case class  UpstreamError(status: Int) extends Exception

  class WunderAPIRequest[A](req: Request[A], authToken: String)
                           (implicit ec: ExecutionContext) extends WrappedRequest[A](req) {
    type Response = (WSResponseHeaders, Enumerator[Array[Byte]])
    val token = authToken

    private def call_base(authToken: String, m: Method): WSRequest =
      WS.url(m.url.toString)
        .withHeaders(
          "X-Access-Token" -> authToken,
          "X-Client-ID" -> Auth.clientId
        )

    def json(m: Method): Future[JsValue] =
      call_base(token, m).get().map {
        case resp if (resp.status >= 200) && (resp.status < 300) =>
          resp.json
        case resp if resp.status == 401 =>
          throw UnauthorizedException
        case resp =>
          throw UpstreamError(resp.status)
      }

    def stream(m: Method): Future[Response] =
      call_base(token, m).getStream().map {
        case resp@(h, body) if  (h.status >= 200) && (h.status < 300) => resp
        case resp@(h, _)    if h.status == 401 =>
          throw UnauthorizedException
        case resp@(h, _) =>
          throw UpstreamError(h.status)
      }
  }

  class WunderAction(token: Option[String] = None) extends ActionBuilder[WunderAPIRequest] with Results {
    implicit val ec = this.executionContext

    override def invokeBlock[A](r: Request[A], block: WunderAPIRequest[A] => Future[Result]) = {
      implicit val req = r
      token.orElse(Auth.token) match {
        case None => Future { authFail }
        case Some(authToken) =>
          val api = new WunderAPIRequest(req, authToken)
          block(api) recover {
            case UnauthorizedException => authFail
            case UpstreamError(status) =>
              Status(status)
            case e: ConnectException =>
              Logger.error("Wunderlist API is unreachable", e)
              ServiceUnavailable
            case e =>
              Logger.error("Wunderlist API error", e)
              InternalServerError
          }
      }
    }
  }

  object WunderAction extends WunderAction(None) {
    def apply(token: String) = new WunderAction(Some(token))
  }
}
