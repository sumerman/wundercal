package utils

import net.fortuna.ical4j.model._
import org.scalatest._
import play.api.libs.iteratee.Enumerator
import play.api.libs.json._
import play.extras.iteratees.CharString

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

  val jsonArray = s"[$json1,$json2,$json1]"
  val parsedArray = Json.parse(jsonArray).as[List[Map[String, JsValue]]]

  val calTitle = "test cal"
  val json2cal = JsonToCalendar.taskListParser(calTitle)
  def applyTaskListParser(arg: String): Calendar = {
    Await.result(
      Enumerator(CharString.fromString(arg)) |>>> json2cal,
      1 second
    )
  }

  "taskListParser" should "return a calendar for valid input" in {
    val res = applyTaskListParser(jsonArray)
    res shouldBe a [Calendar]
    res.getProperty(JsonToCalendar.X_CALNAME).getValue shouldEqual calTitle
  }

  "This calendar" should "contain only tasks with due date" in {
    val res = applyTaskListParser(jsonArray)
    val validEvents = parsedArray.filter(_.get("due_date") != None)
    res.getComponents.size() shouldEqual validEvents.length
    val calEvents = res.getComponents.asScala
    for (obj <- validEvents) {
      val expectedId = obj("id").as[Long].toString
      for (e <- calEvents) {
        val compE = e.asInstanceOf[Component]
        compE.getProperty(Property.UID).getValue should endWith(expectedId)
      }
    }
  }

  // TODO DateParsing
  // TODO DateTime parsing
  // TODO Reminders parsing
  // TODO Tasks with reminders

}
