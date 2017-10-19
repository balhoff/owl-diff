package org.renci.owl.differ

import org.semanticweb.owlapi.model.IRI

import com.typesafe.config.ConfigFactory

import akka.actor.ActorSystem
import akka.http.scaladsl.server.HttpApp
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.settings.ServerSettings
import akka.http.scaladsl.unmarshalling.Unmarshaller

object Main extends HttpApp with App {

  val conf = ConfigFactory.load()
  val serverName = conf.getString("owl-diff.servername")
  val serverPort = conf.getInt("owl-diff.port")

  val system = ActorSystem("owl-diff-system")
  import system.dispatcher

  implicit val IRIUnmarshaller: Unmarshaller[String, IRI] = Unmarshaller.strict(IRI.create)

  def routes: Route =
    pathSingleSlash {
      complete {
        Home.homeForm
      }
    } ~
      path("diff") {
        parameters('left.as[IRI], 'right.as[IRI], 'loadimports.as[Boolean]) { (left, right, loadImports) =>
          complete {
            Differ.loadOntologies(left, right, loadImports).map {
              case (leftOnt, rightOnt) =>
                Differ.diff(leftOnt, rightOnt)
            }
          }
        }
      }

  startServer(host = serverName, port = serverPort, settings = ServerSettings(conf), system)

}