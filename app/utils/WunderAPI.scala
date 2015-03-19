package utils

import java.net.{ConnectException, URL}

import controllers.Auth
import play.api.Logger
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.JsValue
import play.api.mvc.{Request, Action, Result, Results}

import scala.concurrent.{ExecutionContext, Future}

trait WunderAPI extends Results {
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
    case class Tasks(listId: ListID) extends Method {
      def spec = s"./tasks?list_id=${listId.toString}"
    }
    case class Reminders(listId: ListID) extends Method {
      def spec = s"./reminders?list_id=${listId.toString}"
    }
  }

  case object UnauthorizedException extends Exception

  import play.api.libs.ws._

  object WunderAction {
    type Response = (WSResponseHeaders, Enumerator[Array[Byte]])

    type APIFun[T] = Method => Future[T]
    trait API[T] {
      def call: APIFun[T]
      def token: String
    }

    private def call_base(authToken: String, m: Method): WSRequestHolder =
      WS.url(methodURL(m).toString)
        .withHeaders(
          "X-Access-Token" -> authToken,
          "X-Client-ID" -> Auth.clientId
        )

    private def call_json(authToken: String)(m: Method)(implicit ec: ExecutionContext): Future[JsValue] =
      call_base(authToken, m).get().map {
        case resp if resp.status == 200 => resp.json
        case resp if resp.status == 401 =>
          throw UnauthorizedException
      }

    private def call_stream(authToken: String)(m: Method)(implicit ec: ExecutionContext): Future[Response] =
      call_base(authToken, m).getStream().map {
        case resp@(h, body) if h.status == 200 => resp
        case resp@(h, _)    if h.status == 401 =>
          throw UnauthorizedException
      }

    def context_base[T](token: Option[String], wcall: (String => APIFun[T]), f: API[T] => Future[Result])
                       (implicit ec: ExecutionContext) =
      Action.async { implicit req =>
        token.orElse(Auth.token) match {
          case Some(authToken) =>
            val api = new API[T] {
              def call = wcall(authToken)
              def token = authToken
            }
            f(api) recover {
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

    def apply(token: String) = new {
      def json(f: API[JsValue] => Future[Result])(implicit ec: ExecutionContext) =
        context_base(Some(token), call_json, f)(ec)
      def stream(f: API[Response] => Future[Result])(implicit ec: ExecutionContext) =
        context_base(Some(token), call_stream, f)(ec)
    }

    def json(f: API[JsValue] => Future[Result])(implicit ec: ExecutionContext) =
      context_base(None, call_json, f)(ec)
    def stream(f: API[Response] => Future[Result])(implicit ec: ExecutionContext) =
      context_base(None, call_stream, f)(ec)
  }
}
