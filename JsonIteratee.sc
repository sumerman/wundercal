import play.api.libs.iteratee.Enumerator
import play.extras.iteratees.CharString
import utils.JsonToCalendar._
import scala.concurrent.Await
import scala.concurrent.duration._
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

val jsonReminders =
  """
    |[
    |  {
    |    "id": 52257864,
    |    "date": "2015-03-21T01:00:00.000Z",
    |    "task_id": 1055526595,
    |    "revision": 1,
    |    "created_at": "2015-03-22T00:07:54.885Z",
    |    "updated_at": "2015-03-22T00:07:54.885Z",
    |    "type": "reminder"
    |  }
    |]
  """.stripMargin
val json2cal = taskListParser("test cal")
Await.result(
  Enumerator(CharString.fromString(json3)) |>>> json2cal,
  1 second
)

