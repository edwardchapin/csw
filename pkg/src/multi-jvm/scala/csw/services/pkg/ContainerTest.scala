package csw.services.pkg

import akka.actor._
import akka.remote.testkit._
import akka.testkit.ImplicitSender
import com.typesafe.config.ConfigFactory
import csw.services.ccs.akka.CommandServiceActor.Submit
import csw.services.ls.LocationServiceActor
import csw.services.pkg.LifecycleManager.LifecycleStateChanged
import csw.shared.cmd_old.CommandStatus
import csw.util.config.TestConfig

import scala.concurrent.duration._

/**
 * A test that runs each of the classes below and the location service
 * in a separate JVM (See the sbt-multi-jvm plugin).
 * See http://doc.akka.io/docs/akka/current/dev/multi-node-testing.html#multi-node-testing.
 */
object ContainerConfig extends MultiNodeConfig {
  val container1 = role("container1")

  val container2 = role("container2")

  val locationService = role("locationService")

  // We need to configure the location service to run on a known port
  nodeConfig(locationService)(ConfigFactory.load("testLocationService.conf"))
}

class TestMultiJvmContainer1 extends ContainerSpec

class TestMultiJvmContainer2 extends ContainerSpec

class TestMultiJvmLocationService extends ContainerSpec


class ContainerSpec extends MultiNodeSpec(ContainerConfig) with STMultiNodeSpec with ImplicitSender {

  import ContainerConfig._

  override def initialParticipants: Int = roles.size

  "A container" must {

    "wait for all nodes to enter a barrier" in {
      enterBarrier("startup")
    }

    "be able to create a local Assembly and add two remote Hcds" in {
      runOn(container1) {
        enterBarrier("locationServiceStarted")
        enterBarrier("deployed")
        val container = Container.create(ConfigFactory.load("container1.conf"))
        within(10.seconds) {
          container ! Container.GetComponents
          val map = expectMsgType[Container.Components].map
          assert(map.size == 1)
          for((name, assembly1) <- map) {
            assembly1 ! LifecycleManager.SubscribeToLifecycleStates(onlyRunningAndConnected = true)
            val stateChange = expectMsgType[LifecycleStateChanged]
            assert(stateChange.connected && stateChange.state.isRunning)
            assembly1 ! Submit(TestConfig.testConfig)
            val s1 = expectMsgType[CommandStatus.Queued]
            val s2 = expectMsgType[CommandStatus.Busy]
            val s3a = expectMsgType[CommandStatus.PartiallyCompleted]
            val s3 = expectMsgType[CommandStatus.Completed]
            assert(s1.runId == s2.runId)
            assert(s3.runId == s2.runId)
            assert(s3a.runId == s3.runId)
          }
          println("\nContainer1 tests passed\n")
          enterBarrier("done")
          container ! LifecycleManager.Uninitialize
        }
      }

      runOn(container2) {
        enterBarrier("locationServiceStarted")
        val container = Container.create(ConfigFactory.load("container2.conf"))
        container ! Container.GetComponents
        val componentInfo = expectMsgType[Container.Components]
        for ((name, actorRef) <- componentInfo.map) {
          actorRef ! LifecycleManager.SubscribeToLifecycleStates(onlyRunningAndConnected = true)
          val stateChange = expectMsgType[LifecycleStateChanged]
          assert(stateChange.connected && stateChange.state.isRunning)
        }

        println("\nContainer2 tests passed\n")

        enterBarrier("deployed")
        enterBarrier("done")
      }

      runOn(locationService) {
        val ls = system.actorOf(Props[LocationServiceActor], LocationServiceActor.locationServiceName)
        enterBarrier("locationServiceStarted")
        enterBarrier("deployed")
        enterBarrier("done")
      }

      enterBarrier("finished")
    }
  }
}
