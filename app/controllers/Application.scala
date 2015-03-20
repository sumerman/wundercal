package controllers

import play.api.Logger
import play.api.Play._
import play.api.libs.Crypto
import play.api.libs.json._
import play.api.mvc._
import play.extras.iteratees.{Encoding, Combinators}

import utils.{JsonToCalendar, WunderAPI}

import scala.concurrent.Future
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

  type ListId = Long

  def getFoldersReverseIndex(api: WunderAction.API): Future[Map[ListId, String]] =
    api.json(Methods.Folders) map { jsonResp =>
      val jsonFolders = jsonResp.asOpt[List[JsObject]].getOrElse(Nil)
      jsonFolders flatMap { obj =>
        val title = (obj \ "title").asOpt[String].getOrElse("")
        val listIds = (obj \ "list_ids").asOpt[List[ListId]].getOrElse(Nil)
        listIds.map((_, title))
      } toMap
    }

  case class TaskList(id: ListId, name: String)
  def getTaskLists(api: WunderAction.API): Future[List[TaskList]] =
    api.json(Methods.Lists) map { jsonResp =>
      val jsItems = jsonResp.asOpt[List[JsObject]].getOrElse(Nil)
      for {
        obj <- jsItems
        id <- (obj \ "id").asOpt[Long]
        name <- (obj \ "title").asOpt[String]
      } yield TaskList(id, name)
    }

  def index = WunderAction { api =>
    import collection.mutable.{HashMap, Set, MultiMap}
    import scala.collection.JavaConversions._

    getFoldersReverseIndex(api) flatMap { list2folder =>
      getTaskLists(api) map { taskLists =>
        val folders = new HashMap[Option[String], Set[TaskList]]
          with MultiMap[Option[String], TaskList]
        taskLists foreach { tlist =>
          folders.addBinding(list2folder.get(tlist.id), tlist)
        }
        val listsIndex = folders map {
          case (folder, tLists) =>
            val formatted = tLists map {
              case TaskList(id, name) =>
                (name, makeCalendarUrl(id, api.token, api.request))
            }
          (folder, formatted.toMap)
        } toMap
        val noFolder = listsIndex.getOrDefault(None, Map.empty)
        val withFolder = listsIndex.filterNot(_._1.isEmpty) map {
          case (Some(folder), lists) => (folder, mapAsJavaMap(lists))
        }
        Ok(views.html.Application.lists(noFolder, withFolder)) as HTML
      }
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
