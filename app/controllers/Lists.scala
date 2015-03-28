package controllers

import play.api.libs.json._
import play.api.mvc._

import utils.WunderAPI
import scala.concurrent.Future
import scala.language.postfixOps

trait ListsAPIWrappers extends WunderAPI {
  import play.api.libs.concurrent.Execution.Implicits.defaultContext

  type ListId = Long
  protected def getTitle[A](apiReq: WunderAPIRequest[A], listId: ListId): Future[String] =
    apiReq.json(Methods.List(listId)) map { listJs =>
      (listJs \ "title").asOpt[String].getOrElse("")
    }

  case class TaskList(id: ListId, name: String)
  protected def getTaskLists[A](apiReq: WunderAPIRequest[A]): Future[List[TaskList]] =
    apiReq.json(Methods.Lists) map { jsonResp =>
      val jsItems = jsonResp.asOpt[List[JsObject]].getOrElse(Nil)
      for {
        obj <- jsItems
        id <- (obj \ "id").asOpt[Long]
        name <- (obj \ "title").asOpt[String]
      } yield TaskList(id, name)
    }

  protected def getFoldersReverseIndex[A](apiReq: WunderAPIRequest[A]): Future[Map[ListId, String]] =
    apiReq.json(Methods.Folders) map { jsonResp =>
      val jsonFolders = jsonResp.asOpt[List[JsObject]].getOrElse(Nil)
      jsonFolders flatMap { obj =>
        val title = (obj \ "title").asOpt[String].getOrElse("")
        val listIds = (obj \ "list_ids").asOpt[List[ListId]].getOrElse(Nil)
        listIds.map((_, title))
      } toMap
    }
}

object Lists extends Controller with WunderAPI with ListsAPIWrappers {
  import play.api.libs.concurrent.Execution.Implicits.defaultContext

  def authFail = Redirect(routes.Auth.auth()).withNewSession

  def index = WunderAction.async { apiReq =>
    import collection.mutable.{HashMap, Set, MultiMap}
    import scala.collection.JavaConversions._

    getFoldersReverseIndex(apiReq) flatMap { list2folder =>
      getTaskLists(apiReq) map { taskLists =>
        val folders = new HashMap[Option[String], Set[(String, String)]]
          with MultiMap[Option[String], (String, String)]
        taskLists foreach { tList =>
          val item = (tList.name, routes.Lists.details(tList.id, Some(tList.name)).url)
          folders.addBinding(list2folder.get(tList.id), item)
        }
        val listsIndex = folders.toMap.mapValues(_.toList.sorted)
        val noFolder = listsIndex.getOrDefault(None, Nil)
        val withFolder = listsIndex flatMap {
          case (None, _) => Nil
          case (Some(folder), lists) => List((folder, seqAsJavaList(lists)))
        }
        Ok(views.html.Lists.index(noFolder, withFolder)) as HTML
      }
    }
  }

  def details(listId: ListId, maybeListTitle: Option[String] = None) =
    WunderAction.async { apiReq =>
      val listTitleFut = maybeListTitle
        .map(Future(_))
        .getOrElse(getTitle(apiReq, listId))
      listTitleFut map { listTitle =>
        Ok(views.html.Lists.details(listId, listTitle)) as HTML
      }
    }

}
