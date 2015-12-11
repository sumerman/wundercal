package controllers

import play.api.mvc._
import utils.WunderAuth

object Auth extends Controller with WunderAuth {
  def wunderbackRoute = routes.Auth.wunderback()
  def successRoute = routes.Lists.index()
}
