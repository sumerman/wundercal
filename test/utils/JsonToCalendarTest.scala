package utils

import net.fortuna.ical4j.model._
import net.fortuna.ical4j.model.component.VAlarm

import org.scalatest._
import org.scalatest.Inspectors._

import play.api.libs.iteratee.{Iteratee, Enumerator}
import play.api.libs.json._
import play.extras.iteratees.CharString
import utils.JsonToCalendar.AlarmsIndex

import scala.collection.JavaConverters._
import scala.language.postfixOps
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class JsonToCalendarTest extends FlatSpec with Matchers {

  val json1 = """
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
               |  "title": "Сходить в магазин",
               |  "type": "task"
               |}
               |""".stripMargin
  val json2 = """
                |{
                |  "id": 1051180611,
                |  "created_at": "2015-03-18T17:38:35.583Z",
                |  "title": "Foobar",
                |  "type": "task"
                |}
                |""".stripMargin

  val jsonReminders = """
                        |[
                        |  {
                        |    "id": 52257864,
                        |    "date": "2015-03-21T01:00:00.000Z",
                        |    "task_id": 1051180611,
                        |    "revision": 1,
                        |    "created_at": "2015-03-22T00:07:54.885Z",
                        |    "updated_at": "2015-03-22T00:07:54.885Z",
                        |    "type": "reminder"
                        |  }
                        |]
                      """.stripMargin

  val jsonArray = s"[$json1,$json2,$json1]"
  val parsedArray = Json.parse(jsonArray).as[List[Map[String, JsValue]]]
  val parsedReminders = Json.parse(jsonReminders).as[List[Map[String, JsValue]]]

  val calTitle = "test cal"
  val json2cal = JsonToCalendar.taskListParser(calTitle)
  val json2alarmIdx = JsonToCalendar.parseAlarmsToIndex()
  def applyParser[R](parser: Iteratee[CharString, R], arg: String): R = {
    Await.result(
      Enumerator(CharString.fromString(arg)) |>>> parser,
      1 second
    )
  }

  val calendar = applyParser(json2cal, jsonArray)
  "taskListParser" should "return a calendar for valid input" in {

    calendar shouldBe a [Calendar]
    calendar.getProperty(JsonToCalendar.X_CALNAME).getValue shouldEqual calTitle
  }

  val validEvents = parsedArray.filter(_.get("due_date") != None)

  "This calendar" should "contain only tasks with a due date and their properties must be correct" in {
    calendar.getComponents.size() shouldEqual validEvents.length
    val calEvents = calendar.getComponents.asScala
    for (obj <- validEvents) {
      val expectedId = obj("id").as[Long].toString
      val expectedSummary = obj("title").as[String]
      val expectedDateStr = obj("due_date").as[String].filter(_.isDigit)
      for (e <- calEvents) {
        val compE = e.asInstanceOf[Component]
        compE.getProperty(Property.UID).getValue should endWith(expectedId)
        compE.getProperty(Property.DESCRIPTION).getValue should endWith(s"wunderlist://tasks/$expectedId")
        compE.getProperty(Property.SUMMARY).getValue should endWith(expectedSummary)
        compE.getProperty(Property.DTSTART).getValue shouldEqual expectedDateStr
      }
    }
  }

  val alarmsIndex = applyParser(json2alarmIdx, jsonReminders)

  "parseAlarmsToIndex" should "return an AlarmsIndex for valid input" in {
    alarmsIndex shouldBe a [AlarmsIndex]
  }

  it should "contain a mapping from a taskID to a list of alarms" in {
    for(r <- parsedReminders) {
      val expectedId = r("task_id").as[Long].toString
      val expectedTime = JsonToCalendar.reminderDateFormat.parse(r("date").as[String]).getTime
      val alarms = alarmsIndex.get(expectedId)
      alarms should not equal None
      for(a <- alarms.get) println(a.getClass)
      forAll (alarms.get) { alarm =>
        alarm shouldBe a [VAlarm]
        alarm.getTrigger.getDateTime.getTime shouldEqual expectedTime
      }
    }
  }

  // TODO Tasks with reminders (with reminders but without a due must not be there)
  // TODO Reminders noisyfication

}
