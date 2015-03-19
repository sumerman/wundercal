package controllers

import play.api.Play._
import play.api.Logger
import play.api.mvc._

import utils.WunderAPI

object Application extends Controller with WunderAPI {
  import play.api.libs.concurrent.Execution.Implicits.defaultContext

  def authFail = Redirect(routes.Auth.auth()).withNewSession

  def lists = WunderAction { api =>
    api(Methods.Lists) map {
      case body =>
        Ok(body)
    }
  }

  def tasks(listId: Long) = WunderAction.stream { api =>
    api(Methods.Tasks(listId)) map {
      case (_, body)=>
        Ok.feed(body) as JSON
    }
  }

}
