import net.fortuna.ical4j.model.component.VEvent
import play.api.libs.iteratee._
import play.api.libs.json.JsString
import play.extras.iteratees.{CharString, Combinators, Encoding}
import play.extras.iteratees.JsonIteratees._
import play.extras.iteratees.JsonEnumeratees._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import net.fortuna.ical4j.model._
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

val dueFormat = new java.text.SimpleDateFormat("yyyy-MM-dd")

val taskSchema = jsObject { field: String =>
  field match {
    case "id" =>
      jsNumber map { idjs =>
        val idStr = idjs.value.toString()
        val uid = new property.Uid(s"wunderlist-task-$idStr")
        val desc = new property.Description(s" wunderlist://tasks/$idStr")
        List(uid, desc)
      }
    case "due_date" =>
      jsNullOr(jsString) map {
        case None => Nil
        case Some(str) =>
          val javaDate = dueFormat.parse(str.value)
          val calDate = new net.fortuna.ical4j.model.Date(javaDate)
          List(new property.DtStart(calDate))
      }
    case "title" =>
      jsNullOr(jsString) map { topt =>
        List(new property.Summary(topt.getOrElse(JsString("")).value))
      }
    case _ => jsValue.map(_ => Nil)
  }
}

val ignore = Enumeratee.filterNot[List[Property]](_ == Nil)

val buildEvent = Iteratee.fold[List[Property], Option[VEvent]](None) {
  (maybeEvent, props) =>
    lazy val newEvent = new VEvent()
    val event = maybeEvent.getOrElse(newEvent)
    for (prop <- props)
      event.getProperties.add(prop)
    Some(event)
}.map(_.getOrElse(new VEvent()))

val task = taskSchema ><> ignore &>> buildEvent

val ignoreNoDate = Enumeratee.filterNot[VEvent](_.getStartDate == null)

val buildCalendar = Iteratee.fold[VEvent, Option[Calendar]](None) {
  (maybeCal, event) =>
    lazy val newCal = new Calendar()
    val cal = maybeCal.getOrElse(newCal)
    cal.getComponents.add(event)
    Some(cal)
}.map(_.getOrElse(new Calendar()))

val taskList = jsArray(_ => task) ><> ignoreNoDate &>> buildCalendar

// Encoding.decode() ><> Combinators.errorReporter ><>
Await.result(
  Enumerator(CharString.fromString(json3)) |>>> taskList,
  1 second
)

