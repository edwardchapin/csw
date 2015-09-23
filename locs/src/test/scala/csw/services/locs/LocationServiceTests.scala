package csw.services.locs

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import com.typesafe.scalalogging.slf4j.LazyLogging
import csw.services.locs.AccessType.AkkaType
import csw.services.locs.LocationService.{Disconnected, ServicesReady}
import org.scalatest.{BeforeAndAfterAll, FunSuiteLike}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Success, Failure}

class LocationServiceTests extends TestKit(ActorSystem("Test"))
with ImplicitSender with FunSuiteLike with BeforeAndAfterAll with LazyLogging {

  import system.dispatcher

  test("Test location service") {
    val serviceRefs = Set(ServiceRef(ServiceId("TestService", ServiceType.Assembly), AkkaType))
    system.actorOf(LocationService.props(serviceRefs, Some(self)))

    // register
    LocationService.registerAkkaService(serviceRefs.head.serviceId, testActor, "test.prefix")
    within(15.seconds) {
      val ready = expectMsgType[ServicesReady](10.seconds)
      logger.info(s"Services ready: $ready")
      assert(serviceRefs == ready.services.keys)
    }
  }

}

