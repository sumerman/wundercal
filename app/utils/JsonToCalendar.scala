package utils

import scala.collection.JavaConverters._
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

  val X_WTASK_ID_PROP = "X-WTASK-ID"
  val X_CALNAME = "X-WR-CALNAME"
  val reminderDateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")

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
            val calDateStr = str.toString().filter(_.isDigit)
            val params = new ParameterList()
            params.add(parameter.Value.DATE)
            List(new property.DtStart(params, calDateStr))
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
            val dateTime = reminderDateFormat.parse(str.value)
            val calDateTime = new net.fortuna.ical4j.model.DateTime(dateTime)
            Some(new property.Trigger(calDateTime))
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
            newCal.getProperties.add(new XProperty(X_CALNAME, name))
            newCal.getProperties.add(property.Version.VERSION_2_0)
            newCal.getProperties.add(property.CalScale.GREGORIAN)
            newCal
        }
        val taskIdProp = event.getProperty(X_WTASK_ID_PROP)
        if (taskIdProp != null) {
          val taskId = taskIdProp.getValue
          alarmsIndex.get(taskId) match {
            case None =>
            case Some(alarms) =>
              for(a <- alarms) event.getAlarms.add(a)
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

  private def noisifyReminders(count: Int, interval: Long)
                              (implicit ec: ExecutionContext) =
    Enumeratee.mapConcat[VAlarm] { alarm: VAlarm =>
      if (count <= 0) Nil
      else if (count == 1) List(alarm)
      else {
        val additionalAlarms = (1 to count-1).toList map { n =>
          val newAlarm = new VAlarm()
          for (p <- alarm.getProperties.asScala) {
            val prop = p.asInstanceOf[Property]
            newAlarm.getProperties.add(prop.copy())
          }
          val t0 = alarm.getTrigger.getDateTime.getTime
          val t1 = t0 + interval * n
          val newDT = new DateTime(t1)
          newAlarm.getTrigger.setDateTime(newDT)
          newAlarm
        }
        alarm :: additionalAlarms
      }
    }

  type AlarmsIndex = Map[String, List[VAlarm]]
  private def alarms2index(implicit ec: ExecutionContext) =
    Iteratee.fold[VAlarm, AlarmsIndex](Map.empty) {
      (index, alarm) =>
        val idProp = alarm.getProperty(X_WTASK_ID_PROP)
        if (idProp == null) index
        else {
          val key = idProp.getValue
          val prevIdxEntry = index.getOrElse(key, Nil)
          index + (key -> (alarm :: prevIdxEntry))
        }
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
  def parseAlarmsToIndex(count: Int = 1, interval: Long = 0)(implicit ec: ExecutionContext) =
    (jsArray(_ => alarmParser)
      compose getSome
      compose noisifyReminders(count, interval)) transform alarms2index
  
}
