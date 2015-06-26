package utils

import java.net.{ConnectException, URL}

import controllers.Auth
import play.api.Logger
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.JsValue
import play.api.mvc._

import scala.concurrent.Future

trait WunderAPI {
  import play.api.Play.current

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
    case object Folders extends Method {
      def spec = s"./folders"
    }
    case class Tasks(listId: ListID) extends Method {
      def spec = s"./tasks?list_id=${listId.toString}"
    }
    case class Reminders(listId: ListID) extends Method {
      def spec = s"./reminders?list_id=${listId.toString}"
    }
  }

  case object UnauthorizedException extends Exception
  case class  UpstreamError(status: Int) extends Exception

  import play.api.libs.ws._

  type APIFun[T] = Method => Future[T]
  type Response = (WSResponseHeaders, Enumerator[Array[Byte]])

  abstract class WunderAPIRequest[A](req: Request[A]) extends WrappedRequest[A](req) {
    def json: APIFun[JsValue]
    def stream: APIFun[Response]
    def token: String
  }

  class WunderAction(token: Option[String] = None) extends ActionBuilder[WunderAPIRequest] with Results {

    implicit val ec = this.executionContext

    private def call_base(authToken: String, m: Method): WSRequestHolder =
      WS.url(methodURL(m).toString)
        .withHeaders(
          "X-Access-Token" -> authToken,
          "X-Client-ID" -> Auth.clientId
        )

    private def call_json(authToken: String)(m: Method): Future[JsValue] =
      call_base(authToken, m).get().map {
        case resp if (resp.status >= 200) && (resp.status < 300) =>
          resp.json
        case resp if resp.status == 401 =>
          throw UnauthorizedException
        case resp =>
          throw UpstreamError(resp.status)
      }

    private def call_stream(authToken: String)(m: Method): Future[Response] =
      call_base(authToken, m).getStream().map {
        case resp@(h, body) if  (h.status >= 200) && (h.status < 300) => resp
        case resp@(h, _)    if h.status == 401 =>
          throw UnauthorizedException
        case resp@(h, _) =>
          throw UpstreamError(h.status)
      }

    override def invokeBlock[A](r: Request[A], block: WunderAPIRequest[A] => Future[Result]) = {
      implicit val req = r
      token.orElse(Auth.token) match {
        case None => Future { authFail }
        case Some(authToken) =>
          val api = new WunderAPIRequest(req) {
            def json = call_json(authToken)
            def stream = call_stream(authToken)
            def token = authToken
          }
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
