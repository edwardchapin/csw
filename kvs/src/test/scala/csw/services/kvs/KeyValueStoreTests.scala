package csw.services.kvs

import akka.testkit.{ ImplicitSender, TestKit }
import akka.actor.ActorSystem
import csw.util.cfg.Configurations.SetupConfig
import csw.util.cfg.Key
import csw.util.cfg.StandardKeys._
import org.scalatest.{ DoNotDiscover, BeforeAndAfterAll, FunSuiteLike }
import com.typesafe.scalalogging.slf4j.LazyLogging
import scala.concurrent.Await
import scala.concurrent.duration._

object KeyValueStoreTests {

  // Define keys for testing
  val infoValue = Key.create[Int]("infoValue")

  val infoStr = Key.create[String]("infoStr")

  val boolValue = Key.create[Boolean]("boolValue")
}

// Added annotation below, since test depends on Redis server running (Remove to include in tests)
@DoNotDiscover
class KeyValueStoreTests
    extends TestKit(ActorSystem("Test"))
    with ImplicitSender with FunSuiteLike with LazyLogging with BeforeAndAfterAll with Implicits {

  import KeyValueStoreTests._

  implicit val execContext = system.dispatcher
  val kvs = KeyValueStore[SetupConfig]

  test("Test Set and Get") {
    val config1 = SetupConfig("tcs.test")
      .set(infoValue, 1)
      .set(infoStr, "info 1")

    val config2 = SetupConfig("tcs.test")
      .set(infoValue, 2)
      .set(infoStr, "info 2")

    val f = for {
      res1 ← kvs.set("test1", config1)
      val1 ← kvs.get("test1")
      res2 ← kvs.set("test2", config2)
      val2 ← kvs.get("test2")
      res3 ← kvs.delete("test1", "test2")
      res4 ← kvs.get("test1")
      res5 ← kvs.get("test2")
      res6 ← kvs.delete("test1", "test2")
      res7 ← kvs.hmset("testx", config1.getStringMap)
      res8 ← kvs.hmget("testx", infoValue.name)
      res9 ← kvs.hmget("testx", infoStr.name)
    } yield {
      assert(res1)
      assert(val1.exists(_.prefix == "tcs.test"))
      assert(val1.exists(_.get(infoValue).contains(1)))
      assert(val1.exists(_.get(infoStr).contains("info 1")))
      assert(res2)
      assert(val2.exists(_.get(infoValue).contains(2)))
      assert(val2.exists(_.get(infoStr).contains("info 2")))
      assert(res3 == 2)
      assert(res4.isEmpty)
      assert(res5.isEmpty)
      assert(res6 == 0)
      assert(res7)
      assert(res8.contains("1"))
      assert(res9.contains("info 1"))
    }
    Await.result(f, 5.seconds)
  }

  test("Test lset, lget and getHistory") {
    val config = SetupConfig("testPrefix").set(exposureTime, 2)
    val key = "test"
    //    val testKey = "testKey"
    val n = 3

    val f = for {
      _ ← kvs.lset(key, config.set(exposureTime, 3), n)
      _ ← kvs.lset(key, config.set(exposureTime, 4), n)
      _ ← kvs.lset(key, config.set(exposureTime, 5), n)
      _ ← kvs.lset(key, config.set(exposureTime, 6), n)
      _ ← kvs.lset(key, config.set(exposureTime, 7), n)
      v ← kvs.lget(key)
      h ← kvs.getHistory(key, n + 1)
      _ ← kvs.delete(key)
    } yield {
      assert(v.isDefined)
      assert(v.get.get(exposureTime).get == 7.0)
      assert(h.size == n + 1)
      for (i ← 0 to n) {
        logger.info(s"History: $i: ${h(i)}")
      }
    }
    Await.result(f, 5.seconds)
  }

  test("Test usage") {
    val config = SetupConfig("tcs.test")
      .set(infoValue, 2)
      .set(infoStr, "info 2")
      .set(boolValue, true)


    kvs.set("test3", config).onSuccess {
      case result if result =>
        kvs.get("test3").onSuccess {
          case Some(setupConfig) =>
            assert(setupConfig.get(infoValue).get == 2)
            assert(setupConfig.get(infoStr).get == "info 2")
            assert(setupConfig.get(boolValue).get)
            setupConfig
        }
    }
  }

    override def afterAll(): Unit = {
    system.terminate()
  }
}