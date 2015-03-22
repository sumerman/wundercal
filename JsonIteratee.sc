import net.fortuna.ical4j.model.component.VAlarm
import net.fortuna.ical4j.model.{Property, property}
import play.api.libs.iteratee._
import play.api.libs.json.JsString
import play.extras.iteratees.CharString
import play.extras.iteratees.JsonEnumeratees._
import play.extras.iteratees.JsonIteratees._
import utils.JsonToCalendar._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps
val json = """
             |{
             |  "id": 1051180611,
             |  "created_at": "2015-03-18T17:38:35.583Z",
             |  "recurrence_type": "day",
             |  "recurrence_count": 1,
             |  "due_date": "2015-03-19",
             |  "completed": false,
             |  "starred": false,
             |  "list_id": 121063920,
             |  "revision": 1,
             |  "title": "Заправить диван",
             |  "type": "task"
             |}
             |""".stripMargin
val json2 = """
              |{
              |  "id": 1051180611,
              |  "created_at": "2015-03-18T17:38:35.583Z",
              |  "title": "Жепь",
              |  "type": "task"
              |}
              |""".stripMargin
val json3 = s"[$json,$json2,$json]"
val jsonReminder =
  """
    |  {
    |    "id": 52257864,
    |    "date": "2015-03-21T01:00:00.000Z",
    |    "task_id": 1055526595,
    |    "revision": 1,
    |    "created_at": "2015-03-22T00:07:54.885Z",
    |    "updated_at": "2015-03-22T00:07:54.885Z",
    |    "type": "reminder"
    |  }
  """.stripMargin
val jsonReminders = s"[$jsonReminder]"
private val X_WTASK_ID_PROP = "X-WTASK-ID"
private val dateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")
val reminderSchema = jsObject { field: String =>
  field match {
    case "id" =>
      jsNumber map { idjs =>
        val idStr = idjs.value.toString()
        val uid = new property.Uid(s"wunderlist-reminder-$idStr")
        Some(uid)
      }
    case "task_id" =>
      jsNumber map { idjs =>
        val idStr = idjs.value.toString()
        val tid = new property.XProperty(X_WTASK_ID_PROP, idStr)
        Some(tid)
      }
    case "date" =>
      jsNullOr(jsString) map {
        case None => None
        case Some(str) =>
          val dateTime = dateFormat.parse(str.value)
          val calDate = new net.fortuna.ical4j.model.DateTime(dateTime)
          Some(new property.Trigger(calDate))
      }
    case _ => jsValue.map(_ => None)
  }
}
val props2alarms = Iteratee.fold[Property, Option[VAlarm]](None) {
  (maybeAlarm, prop) =>
    val alarm = maybeAlarm match {
      case Some(a) => a
      case None => new VAlarm()
    }
    alarm.getProperties.add(prop)
    Some(alarm)
}
val alarms2index = Iteratee.fold[VAlarm, Map[Long, VAlarm]](Map.empty) {
  (index, alarm) =>
    val taskId = alarm.getProperty(X_WTASK_ID_PROP).getValue.toLong
    index + (taskId -> alarm)
}
def getSome[A] = Enumeratee.filterNot[Option[A]](_.isEmpty) ><> Enumeratee.map[Option[A]](_.get)
val alarmParser = reminderSchema ><> getSome &>> props2alarms
val parseAlarmsToIndex = jsArray(_ => alarmParser) ><> getSome &>> alarms2index
Await.result(
  Enumerator(CharString.fromString(jsonReminders)) |>>> parseAlarmsToIndex,
  1 second
)
