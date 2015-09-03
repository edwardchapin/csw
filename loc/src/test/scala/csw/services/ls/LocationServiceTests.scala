package csw.services.ls

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri
import akka.testkit.{ImplicitSender, TestKit}
import csw.services.ls.ServiceType.{Assembly, HCD}
import org.scalatest.{BeforeAndAfterAll, FunSuiteLike}

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * Simple standalone test of local location service
 */
class LocationServiceTests extends TestKit(ActorSystem("Test"))
with ImplicitSender with FunSuiteLike with BeforeAndAfterAll {

  import system.dispatcher

  val loc = LocationService(system)
  val timeout = 5.seconds

  test("Test location service") {
    val serviceId = ServiceId("TestActor", HCD)

    val regInfo = RegInfo(serviceId,
      configPath = Some("test.path"),
      httpUri = Some(Uri("http://localhost/test"))
    )

    def compareRegInfo(r: RegInfo): Unit = {
      assert(r.serviceId == serviceId)
      assert(r.configPath.contains("test.path"))
      assert(r.httpUri.contains(Uri("http://localhost/test")))
      assert(r.actorUri.isEmpty)
    }

    // Test set/get
    val f = for {
      _ <- loc.register(regInfo)
      opt <- loc.resolve(serviceId, wait = false)
    } yield opt

    Await.result(f, timeout) match {
      case None => fail("Test1 failed")
      case Some(r) => compareRegInfo(r)
    }

    // Test browse
    val l = Await.result(loc.browse(Some("TestActor"), None), timeout)
    assert(l.size == 1)
    l.foreach(compareRegInfo)

    val l2 = Await.result(loc.browse(Some("TestActorXXX"), None), timeout)
    assert(l2.isEmpty)

    val l3 = Await.result(loc.browse(None, Some(HCD)), timeout)
    assert(l3.size == 1)
    l.foreach(compareRegInfo)

    val l4 = Await.result(loc.browse(Some("TestActor"), Some(Assembly)), timeout)
    assert(l4.isEmpty)

    val l5 = Await.result(loc.browse(Some("TestActor"), Some(HCD)), timeout)
    assert(l5.size == 1)
    l.foreach(compareRegInfo)

  }
}
