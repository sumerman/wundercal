package controllers

import play.api.libs.Crypto
import play.api.libs.json._
import play.api.mvc._

import utils.{JsonToCalendar, WunderAPI}

import scala.concurrent.Future
import scala.language.postfixOps
import scala.util.{Try, Random}

object Application extends Controller with WunderAPI {
  import play.api.libs.concurrent.Execution.Implicits.defaultContext

  def authFail = Redirect(routes.Auth.auth()).withNewSession

  private def encodeTaskListUrl(id: Long, token: String): String = {
    val salt = Crypto.sign(s"$id+$token")
    val salt1 = salt.substring(0, Random.nextInt(salt.length-5)+5)
    val string = s"$id+$salt1+$token+$salt1"
    Crypto.encryptAES(string)
  }

  private def decodeTaskListUrl(str: String): Option[(Long, String)] = {
    val decStr = Crypto.decryptAES(str)
    decStr.split("\\+") match {
      case Array(id, _, token, _) =>
        Some((id.toLong, token))
      case _ => None
    }
  }

  private def makeCalendarUrl(id: Long, tok: String, req: Request[_]): String = {
    val calId = encodeTaskListUrl(id, tok)
    val hostRelative = routes.Application.tasksCalendar(calId).url
    s"webcal://${req.host}$hostRelative"
  }

  type ListId = Long

  private def getFoldersReverseIndex[A](apiReq: WunderAPIRequest[A]): Future[Map[ListId, String]] =
    apiReq.json(Methods.Folders) map { jsonResp =>
      val jsonFolders = jsonResp.asOpt[List[JsObject]].getOrElse(Nil)
      jsonFolders flatMap { obj =>
        val title = (obj \ "title").asOpt[String].getOrElse("")
        val listIds = (obj \ "list_ids").asOpt[List[ListId]].getOrElse(Nil)
        listIds.map((_, title))
      } toMap
    }

  case class TaskList(id: ListId, name: String)
  private def getTaskLists[A](apiReq: WunderAPIRequest[A]): Future[List[TaskList]] =
    apiReq.json(Methods.Lists) map { jsonResp =>
      val jsItems = jsonResp.asOpt[List[JsObject]].getOrElse(Nil)
      for {
        obj <- jsItems
        id <- (obj \ "id").asOpt[Long]
        name <- (obj \ "title").asOpt[String]
      } yield TaskList(id, name)
    }

  def index = WunderAction.async { apiReq =>
    import collection.mutable.{HashMap, Set, MultiMap}
    import scala.collection.JavaConversions._

    getFoldersReverseIndex(apiReq) flatMap { list2folder =>
      getTaskLists(apiReq) map { taskLists =>
        val folders = new HashMap[Option[String], Set[(String, String)]]
          with MultiMap[Option[String], (String, String)]
        taskLists foreach { tList =>
          val item = (tList.name, routes.Application.listDetails(tList.id).url)
          folders.addBinding(list2folder.get(tList.id), item)
        }
        val listsIndex = folders.toMap.mapValues(_.toList.sorted)
        val noFolder = listsIndex.getOrDefault(None, Nil)
        val withFolder = listsIndex flatMap {
          case (None, _) => Nil
          case (Some(folder), lists) => List((folder, seqAsJavaList(lists)))
        }
        Ok(views.html.Application.lists(noFolder, withFolder)) as HTML
      }
    }
  }

  private def getListTitle[A](apiReq: WunderAPIRequest[A], listId: ListId): Future[String] =
    apiReq.json(Methods.List(listId)) map { listJs =>
      (listJs \ "title").asOpt[String].getOrElse("")
    }

  private def getAlarmsIndex[A](apiReq: WunderAPIRequest[A], listId: ListId): Future[JsonToCalendar.AlarmsIndex] =
    apiReq.stream(Methods.Reminders(listId)) flatMap {
      case (_, remindersJs) =>
        import JsonToCalendar._
        remindersJs.run(bytes2string transform parseAlarmsToIndex)
    }

  def tasksCalendar(taskListUrl: String) = decodeTaskListUrl(taskListUrl) match {
    case None =>
      Action(BadRequest("Invalid task list calendar id"))
    case Some((listId, token)) => WunderAction(token).async { apiReq =>
      import JsonToCalendar._
      for {
        title <- getListTitle(apiReq, listId)
        alarmsIdx <- getAlarmsIndex(apiReq, listId)
        (_, tasksJs) <- apiReq.stream(Methods.Tasks(listId))
        json2cal = taskListParser(title, alarmsIdx)
        cal <- tasksJs.run(bytes2string transform json2cal)
      } yield Ok(cal.toString).as("text/calendar")
    }
  }

  def listDetails(listId: ListId) = WunderAction.async { apiReq =>
    getListTitle(apiReq, listId) map { listTitle =>
      Ok(views.html.Application.listDetails(listId, listTitle)) as HTML
    }
  }

  object PostKeys {
    val LIST_ID = "list_id"
    val REMINDERS_COUNT = "reminders_count"
    val REMINDERS_INTERVAL = "reminders_interval"
  }

  private def getIntFromForm(key: String, form: Map[String, Seq[String]]): Option[Long] =
    for {
      seq <- form.get(key)
      str <- seq.headOption
      int <- Try(str.toLong).toOption
    } yield int

  def genCalURI() = WunderAction { apiReq =>
    val res = for {
      params <- apiReq.body.asFormUrlEncoded
      listId <- getIntFromForm(PostKeys.LIST_ID, params)
      remindersCnt <- getIntFromForm(PostKeys.REMINDERS_COUNT, params)
      remindersInt <- getIntFromForm(PostKeys.REMINDERS_INTERVAL, params)
      remindersCnt >= 0
      remindersInt > 0
    } yield Redirect(makeCalendarUrl(listId, apiReq.token, apiReq))
    res.getOrElse(BadRequest)
  }

}
