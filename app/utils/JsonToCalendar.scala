package utils

import scala.concurrent.ExecutionContext

object JsonToCalendar {
  import net.fortuna.ical4j.model.component.VEvent
  import play.api.libs.iteratee._
  import play.api.libs.json.JsString
  import play.extras.iteratees.JsonIteratees._
  import play.extras.iteratees.JsonEnumeratees._
  import play.extras.iteratees.{Encoding, Combinators}
  
  import net.fortuna.ical4j.model._
  import net.fortuna.ical4j.model.property.XProperty
  import net.fortuna.ical4j.model.component.VAlarm

  private val dueFormat = new java.text.SimpleDateFormat("yyyy-MM-dd")
  private val X_WTASK_ID_PROP = "X-WTASK-ID"
  private val dateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")

  private def taskSchema(implicit ec: ExecutionContext) = jsObject { field: String =>
    field match {
      case "id" =>
        jsNumber map { idjs =>
          val idStr = idjs.value.toString()
          val uid = new property.Uid(s"wunderlist-task-$idStr")
          val desc = new property.Description(s" wunderlist://tasks/$idStr")
          val tid = new property.XProperty(X_WTASK_ID_PROP, idStr)
          List(uid, desc, tid)
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

  def reminderSchema(implicit ec: ExecutionContext) = jsObject { field: String =>
    field match {
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

  private def getSome[A](implicit ec: ExecutionContext) = Enumeratee.filterNot[Option[A]](_.isEmpty) ><> Enumeratee.map[Option[A]](_.get)
  private def skipEmptyProps(implicit ec: ExecutionContext) = Enumeratee.filterNot[List[Property]](_ == Nil)
  private def ignoreNoDate(implicit ec: ExecutionContext) = Enumeratee.filterNot[VEvent](_.getStartDate == null)

  // VEvent and Calendar is wrapped into an Option
  // because otherwise a mutable object passed into a fold
  // as an initial state lingers during multiple usages.

  private def props2task(implicit ec: ExecutionContext) =
    Iteratee.fold[List[Property], Option[VEvent]](None) {
      (maybeEvent, props) =>
        val event = maybeEvent match {
          case None => new VEvent()
          case Some(e) => e
        }
        for (prop <- props)
          event.getProperties.add(prop)
        Some(event)
    }.map(_.getOrElse(new VEvent()))

  private def events2calendar(name: String, alarmsIndex: AlarmsIndex)
                             (implicit ec: ExecutionContext) =
    Iteratee.fold[VEvent, Option[Calendar]](None) {
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
        val taskIdProp = event.getProperty(X_WTASK_ID_PROP)
        if (taskIdProp != null) {
          val taskId = taskIdProp.getValue
          alarmsIndex.get(taskId) match {
            case None =>
            case Some(alarm) =>
              event.getAlarms.add(alarm)
          }
        }
        cal.getComponents.add(event)
        Some(cal)
    }.map(_.getOrElse(new Calendar()))

  private def props2alarms(implicit ec: ExecutionContext) =
    Iteratee.fold[Property, Option[VAlarm]](None) {
      (maybeAlarm, prop) =>
        val alarm = maybeAlarm match {
          case Some(a) => a
          case None =>
            val alm = new VAlarm()
            alm.getProperties.add(property.Action.DISPLAY)
            alm.getProperties.add(property.Action.AUDIO)
            alm
        }
        alarm.getProperties.add(prop)
        Some(alarm)
    }

  // In current state of Wunderlist there is
  // at most one reminder per task
  type AlarmsIndex = Map[String, VAlarm]
  private def alarms2index(implicit ec: ExecutionContext) =
    Iteratee.fold[VAlarm, AlarmsIndex](Map.empty) {
      (index, alarm) =>
        val idProp = alarm.getProperty(X_WTASK_ID_PROP)
        if (idProp == null) index
        else index + (idProp.getValue -> alarm)
    }

  def bytes2string = Encoding.decode() compose Combinators.errorReporter

  def taskParser(implicit ec: ExecutionContext) =
    (taskSchema compose skipEmptyProps) transform props2task
  def taskListParser(name: String = "", alarmsIndex: AlarmsIndex = Map.empty)
                    (implicit ec: ExecutionContext) =
    (jsArray(_ => taskParser) compose ignoreNoDate)
      .transform(events2calendar(name, alarmsIndex))

  def alarmParser(implicit ec: ExecutionContext) =
    (reminderSchema compose getSome) transform props2alarms
  def parseAlarmsToIndex(implicit ec: ExecutionContext) =
    (jsArray(_ => alarmParser) compose getSome) transform alarms2index
  
}
