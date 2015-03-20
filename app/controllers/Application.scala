package controllers

import com.google.common.base.Charsets
import com.google.common.io.BaseEncoding
import play.api.Logger
import play.api.Play._
import play.api.libs.Crypto
import play.api.libs.json._
import play.api.mvc._
import play.extras.iteratees.{Encoding, Combinators}

import utils.{JsonToCalendar, WunderAPI}

import scala.util.Random

object Application extends Controller with WunderAPI {
  import play.api.libs.concurrent.Execution.Implicits.defaultContext

  def authFail = Redirect(routes.Auth.auth()).withNewSession

  def encodeTaskListUrl(id: Long, token: String): String = {
    val salt = Crypto.sign(s"$id+$token")
    val salt1 = salt.substring(0, Random.nextInt(salt.length-5)+5)
    val string = s"$id+$salt1+$token+$salt1"
    Crypto.encryptAES(string)
  }

  def decodeTaskListUrl(str: String): Option[(Long, String)] = {
    val decStr = Crypto.decryptAES(str)
    decStr.split("\\+") match {
      case Array(id, _, token, _) =>
        Some((id.toLong, token))
      case _ => None
    }
  }

  def makeCalendarUrl(id: Long, tok: String, req: Request[_]): String = {
    val calId = encodeTaskListUrl(id, tok)
    val hostRelative = routes.Application.tasksCalendar(calId).url
    s"webcal://${req.host}$hostRelative"
  }

  def lists = WunderAction { api =>
    import scala.collection.JavaConversions._

    api.json(Methods.Lists) map { body =>
      val maybeItems = body.asOpt[List[JsObject]] map { arr =>
        arr.map({ obj =>
          for {
            id <- (obj \ "id").asOpt[Long]
            name <- (obj \ "title").asOpt[String]
          } yield (name, makeCalendarUrl(id, api.token, api.request))
        }).flatten
      }
      val items = maybeItems.getOrElse(Nil)
      val content = views.html.Application.lists(items)
      Ok(content) as HTML
    }
  }

  def tasksCalendar(taskListUrl: String) = decodeTaskListUrl(taskListUrl) match {
    case None => Action { BadRequest("Invalid task list calendar id") }
    case Some((listId, token)) => WunderAction(token) { api =>
      api.json(Methods.List(listId)) flatMap { listJs =>
        val title = (listJs \ "title").asOpt[String].getOrElse("")
        api.stream(Methods.Tasks(listId)) flatMap {
          case (_, body) =>
            val json2cal = JsonToCalendar.taskListParser(title)
            val parser = Encoding.decode() ><> Combinators.errorReporter &>> json2cal
            body |>>> parser map { cal => Ok(cal.toString).as("text/calendar") }
        }
      }
    }
  }

}
