package csw.services.ccs

import akka.actor._
import akka.testkit.{ImplicitSender, TestKit}
import com.typesafe.scalalogging.slf4j.LazyLogging
import csw.services.ccs.HcdController.Submit
import csw.util.cfg.Configurations.SetupConfig
import csw.util.cfg.StateVariable.CurrentState
import csw.util.cfg.StandardKeys.position
import org.scalatest.FunSuiteLike

import scala.concurrent.duration._

object HcdControllerTests {
  val system = ActorSystem("Test")

  val testPrefix1 = "wfos.blue.filter"
  val testPrefix2 = "wfos.red.filter"

  object TestHcdController {
    def props(): Props = Props(classOf[TestHcdController])
  }

  class TestHcdController extends HcdController with Actor with ActorLogging {

    // Use single worker actor to do work in the background
    // (could also use a worker per job/message if needed)
    val worker = context.actorOf(TestWorker.props())

    // Send the config to the worker for processing
    override protected def process(config: SetupConfig): Unit = {
      worker ! config
    }

    // Ask the worker actor to send us the current state (handled by parent trait)
    override protected def requestCurrent(): Unit = {
      worker ! TestWorker.RequestCurrentState
    }

    override def receive: Receive = controllerReceive
  }

  // -- Test worker actor that simulates doing some work --
  object TestWorker {
    def props(): Props = Props(classOf[TestWorker])

    // Message sent to self to simulate work done
    case class WorkDone(config: SetupConfig)

    // Message to request the current state values
    case object RequestCurrentState
  }

  class TestWorker extends Actor with ActorLogging {

    import TestWorker._
    import context.dispatcher

    // Simulate getting the initial state from the device
    val initialState = CurrentState(testPrefix1).set(position, "None")

    // Simulated current state
    var currentState = initialState

    def receive: Receive = {
      case config: SetupConfig ⇒
        // Simulate doing work
        log.info(s"Start processing $config")
        context.system.scheduler.scheduleOnce(2.seconds, self, WorkDone(config))

      case RequestCurrentState ⇒
        log.info(s"Requested current state")
        context.parent ! currentState

      case WorkDone(config) ⇒
        log.info(s"Done processing $config")
        currentState = CurrentState(config.prefix, config.data)
        context.parent ! currentState

      case x ⇒ log.error(s"Unexpected message $x")
    }
  }

}

// Tests sending a DemandState to a test HCD, then starting a matcher actor to subscribe
// to the current state (a state variable updated by the HCD). When the current state matches
// the demand state, the matcher actor replies with a message (containing the current state).

// Test requires that Redis is running externally
//@DoNotDiscover
class HcdControllerTests extends TestKit(HcdControllerTests.system)
    with ImplicitSender with FunSuiteLike with LazyLogging {

  import HcdControllerTests._

  test("Test non-periodic HCD controller") {
    val hcdController = system.actorOf(TestHcdController.props())

    // Send a setup config to the HCD
    val config = SetupConfig(testPrefix2).set(position, "IR3")
    hcdController ! Submit(config)
    system.actorOf(HcdStatusMatcherActor.props(List(config), Set(hcdController), self))
    within(10.seconds) {
      val status = expectMsgType[CommandStatus.Completed]
      logger.info(s"Done (2). Received reply from matcher with current state: $status")
    }
  }
}
