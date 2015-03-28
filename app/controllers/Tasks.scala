package controllers

import play.api.libs.Crypto
import play.api.mvc.{Request, Action, Controller}
import utils.WunderAPI

import scala.concurrent.Future
import scala.util.{Try, Random}

object Tasks extends Controller with WunderAPI with ListsAPIWrappers {
  import utils._
  import play.api.libs.concurrent.Execution.Implicits.defaultContext

  case class ListCalDescriptor(id: ListId, wlToken: String,
                               remindersCount: Int, remindersRepeatInterval: Int)

  private def encodeTaskListUrl(list: ListCalDescriptor): String =
    list match {
      case ListCalDescriptor(id, token, remCount, remInt) =>
        val salt = Crypto.sign(s"$id+$token")
        val salt1 = salt.substring(0, Random.nextInt(salt.length-5)+5)
        val string = s"$id+$salt1+$token+$salt1+$remCount+$remInt"
        Crypto.encryptAES(string)
    }

  private def decodeTaskListUrl(str: String): Option[ListCalDescriptor] = {
    val decStr = Crypto.decryptAES(str)
    decStr.split("\\+") match {
      case Array(idStr, _, token, _, remCntStr, remIntStr) =>
        for {
          id <- Try(idStr.toLong).toOption
          remCnt <- Try (remCntStr.toInt).toOption
          remInt <- Try (remIntStr.toInt).toOption
        } yield ListCalDescriptor(id, token, remCnt, remInt)
      case _ => None
    }
  }

  private def getIntFromForm(key: String, form: Map[String, Seq[String]]): Option[Long] =
    for {
      seq <- form.get(key)
      str <- seq.headOption
      int <- Try(str.toLong).toOption
    } yield int

  private def makeCalendarUrl(listCalDescriptor: ListCalDescriptor, req: Request[_]): String = {
    val calId = encodeTaskListUrl(listCalDescriptor)
    val hostRelative = routes.Tasks.asCalendar(calId).url
    s"webcal://${req.host}$hostRelative"
  }

  def authFail = Redirect(routes.Auth.auth()).withNewSession

  private def getAlarmsIndex[A](apiReq: WunderAPIRequest[A], listDesc: ListCalDescriptor): Future[JsonToCalendar.AlarmsIndex] =
    apiReq.stream(Methods.Reminders(listDesc.id)) flatMap {
      case (_, remindersJs) =>
        import JsonToCalendar._
        remindersJs.run(bytes2string transform parseAlarmsToIndex(listDesc.remindersCount, listDesc.remindersRepeatInterval))
    }

  def asCalendar(taskListUrl: String) = decodeTaskListUrl(taskListUrl) match {
    case None =>
      Action(BadRequest("Invalid task list calendar id"))
    case Some(listDesc) =>
      WunderAction(listDesc.wlToken).async { apiReq =>
        import JsonToCalendar._
        for {
          title <- getTitle(apiReq, listDesc.id)
          alarmsIdx <- getAlarmsIndex(apiReq, listDesc)
          (_, tasksJs) <- apiReq.stream(Methods.Tasks(listDesc.id))
          json2cal = taskListParser(title, alarmsIdx)
          cal <- tasksJs.run(bytes2string transform json2cal)
        } yield Ok(cal.toString).as("text/calendar")
      }
  }

  object PostKeys {
    val LIST_ID = "list_id"
    val REMINDERS_COUNT = "reminders_count"
    val REMINDERS_INTERVAL = "reminders_interval"
  }

  def genCalURI() = WunderAction { apiReq =>
    val res = for {
      params <- apiReq.body.asFormUrlEncoded
      listId <- getIntFromForm(PostKeys.LIST_ID, params)
      remindersCnt <- getIntFromForm(PostKeys.REMINDERS_COUNT, params)
      remindersInt <- getIntFromForm(PostKeys.REMINDERS_INTERVAL, params)
      if remindersCnt >= 0
      if remindersInt > 0
      repeatInt = remindersInt * 60000
      calDesc = ListCalDescriptor(listId, apiReq.token, remindersCnt.toInt, repeatInt.toInt)
    } yield Redirect(makeCalendarUrl(calDesc, apiReq))
    res.getOrElse(BadRequest)
  }

}
