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

object Application extends Controller with WunderAPI {
  import play.api.libs.concurrent.Execution.Implicits.defaultContext

  def authFail = Redirect(routes.Auth.auth()).withNewSession

  def encodeTaskListUrl(id: Long, token: String): String = {
    val string = s"$id+:$token"
    val encrypted = Crypto.encryptAES(string)
    BaseEncoding.base64Url().encode(encrypted.getBytes(Charsets.UTF_8))
  }

  def decodeTaskListUrl(str: String): Option[(Long, String)] = {
    val encBytes = BaseEncoding.base64Url().decode(str)
    val decStr = Crypto.decryptAES(new String(encBytes))
    decStr.split("\\+:") match {
      case Array(id, token) =>
        Some((id.toLong, token))
      case _ => None
    }
  }

  def lists = WunderAction.json { api =>
    api.call(Methods.Lists) map { body =>
      val maybeItems = body.asOpt[List[JsObject]] map { arr =>
        arr map { obj =>
          for {
            id <- (obj \ "id").asOpt[Long]
            name <- (obj \ "title").asOpt[String]
          } yield (name, encodeTaskListUrl(id, api.token))
        }
      }
      maybeItems.map(is => Ok(is.toString)).getOrElse(BadGateway)
    }
  }

  def tasksCalendar(taskListUrl: String) = decodeTaskListUrl(taskListUrl) match {
    case None => Action { BadRequest("Invalid task list calendar id") }
    case Some((listId, token)) => WunderAction(token).stream { api =>
      api.call(Methods.Tasks(listId)) flatMap {
        case (_, body)=>
          val parser = Encoding.decode() ><> Combinators.errorReporter &>> JsonToCalendar.taskListParser
          body |>>> parser map { cal => Ok(cal.toString).as("text/calendar") }
      }
    }
  }

}
