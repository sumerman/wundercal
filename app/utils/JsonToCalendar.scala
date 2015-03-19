package utils

import net.fortuna.ical4j.model.property.XProperty

object JsonToCalendar {
  import net.fortuna.ical4j.model.component.VEvent
  import play.api.libs.iteratee._
  import play.api.libs.json.JsString
  import play.extras.iteratees.JsonIteratees._
  import play.extras.iteratees.JsonEnumeratees._
  import scala.concurrent.ExecutionContext.Implicits.global
  
  import net.fortuna.ical4j.model._

  private val dueFormat = new java.text.SimpleDateFormat("yyyy-MM-dd")

  private val taskSchema = jsObject { field: String =>
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

  private val skipEmptyProps = Enumeratee.filterNot[List[Property]](_ == Nil)
  private val ignoreNoDate = Enumeratee.filterNot[VEvent](_.getStartDate == null)

  // VEvent and Calendar is wrapped into an Option
  // because otherwise a mutable object passed into a fold
  // as an initial state lingers during multiple usages.

  private val props2task = Iteratee.fold[List[Property], Option[VEvent]](None) {
    (maybeEvent, props) =>
      lazy val newEvent = new VEvent()
      val event = maybeEvent.getOrElse(newEvent)
      for (prop <- props)
        event.getProperties.add(prop)
      Some(event)
  }.map(_.getOrElse(new VEvent()))

  private def events2calendar(name: String) = Iteratee.fold[VEvent, Option[Calendar]](None) {
    (maybeCal, event) =>
      val cal = maybeCal match {
        case Some(c) => c
        case None =>
          val newCal = new Calendar()
          newCal.getProperties.add(new property.Name(name))
          newCal.getProperties.add(new XProperty("X-WR-CALNAME", name))
          newCal.getProperties.add(property.Version.VERSION_2_0)
          newCal.getProperties.add(property.CalScale.GREGORIAN)
          newCal
      }
      cal.getComponents.add(event)
      Some(cal)
  }.map(_.getOrElse(new Calendar()))

  val taskParser = (taskSchema compose skipEmptyProps) transform props2task
  def taskListParser(name: String = "") = (jsArray(_ => taskParser) compose ignoreNoDate) transform events2calendar(name)
  
}
