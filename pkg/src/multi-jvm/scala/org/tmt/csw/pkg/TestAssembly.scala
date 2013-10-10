package scala.org.tmt.csw.pkg

import org.tmt.csw.pkg.Assembly
import akka.actor.Props

object TestAssembly {
  def props(name: String): Props = Props(classOf[TestAssembly], name)
}

// A test assembly
case class TestAssembly(name: String) extends Assembly {

  def receive: Receive = receiveAssemblyMessages

  def initialize(): Unit = {log.info("Assembly1 initialize")}

  def startup(): Unit = {log.info("Assembly1 startup")}

  def run(): Unit = {log.info("Assembly1 run")}

  def shutdown(): Unit = {log.info("Assembly1 shutdown")}

  def uninit(): Unit = {log.info("Assembly1 uninit")}

  def remove(): Unit = {log.info("Assembly1 remove")}
}
