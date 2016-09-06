package csw.examples.vsliceJava.hcd;

import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import akka.testkit.JavaTestKit;
import akka.util.Timeout;
import csw.services.loc.LocationService;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import scala.concurrent.duration.FiniteDuration;
import csw.examples.vsliceJava.hcd.MotionWorker.MotionWorkerMsgs;

import java.util.Collections;
import java.util.Vector;
import java.util.concurrent.TimeUnit;

import static javacsw.util.config.JUnitsOfMeasure.seconds;

public class SingleAxisSimulatorTests {
  private static ActorSystem system;
  Timeout timeout = Timeout.durationToTimeout(FiniteDuration.apply(60, TimeUnit.SECONDS));

  @BeforeClass
  public static void setup() {
    LocationService.initInterface();
    system = ActorSystem.create();
  }

  @AfterClass
  public static void teardown() {
    JavaTestKit.shutdownActorSystem(system);
    system = null;
  }

  Vector<MotionWorkerMsgs> expectLLMoveMsgs(boolean diagFlag) {
    Vector<MotionWorkerMsgs> allMsgs = new Vector<>();
    new JavaTestKit(system) {{
      // Get AxisStarted
      allMsgs.add(expectMsgEquals(MotionWorker.Start.instance));
      // Receive updates until axis idle then get the last one
      final MotionWorkerMsgs[] moveMsgs =
        new ReceiveWhile<MotionWorker.Tick>(MotionWorker.Tick.class, duration("5 second")) {
          protected MotionWorker.Tick match(Object in) {
            if (in instanceof MotionWorker.Tick) {
              return (MotionWorker.Tick)in;
            } else {
              throw noMatch();
            }
          }
        }.get(); // this extracts the received messages

      MotionWorkerMsgs endMsg = expectMsgClass(MotionWorker.End.class); // last one
      Collections.addAll(allMsgs, moveMsgs);
      allMsgs.add(endMsg);

      if (diagFlag) System.out.println("LLMoveMsgs: " + allMsgs);
    }};
    return allMsgs;
  }



  @Test
  public void xxx() throws Exception {

  }

}

//  def expectMoveMsgs(diagFlag: Boolean = false): Seq[AxisUpdate] = {
//    // Get AxisStarted
//    expectMsg(AxisStarted)
//    // Receive updates until axis idle then get the last one
//    val msgs = receiveWhile(5.seconds) {
//      case m@AxisUpdate(_, axisState, current, _, _, _) if axisState == AXIS_MOVING => m
//    }
//    val fmsg = expectMsgClass(classOf[AxisUpdate]) // last one
//    val allmsgs = msgs :+ fmsg
//    if (diagFlag) info(s"MoveMsgs: $allmsgs")
//    allmsgs
//  }
//
//  def expectMoveMsgsWithDest(target: Int, diagFlag: Boolean = false): Seq[AxisResponse] = {
//    // Receive updates until axis idle then get the last one
//    val msgs  = receiveWhile(5.seconds) {
//      case as@AxisStarted => as
//      case m@AxisUpdate(_, currentState, current, _, _, _) if current != target => m
//    }
//    val fmsg1 = expectMsgClass(classOf[AxisUpdate]) // last one when target == current
//    val fmsg2 = expectMsgClass(classOf[AxisUpdate]) // then the End event with the IDLE
//    val allmsgs = msgs :+ fmsg1 :+ fmsg2
//    if (diagFlag) info(s"MoveMsgs: $allmsgs")
//    allmsgs
//  }
//
//  // Calculates the time to wait for messages with a little extra
//  def calcDelay(numberSteps: Int, delayInSseconds: Int): FiniteDuration = (numberSteps + 1) * delayInSseconds * 1000.seconds
//
//  describe("Testing steps calc") {
//
//    // Note that putting functions in the companion object allows them to be easily tested!
//    it("should calculate different number of steps based on the size of the move") {
//      calcNumSteps(100, 105) should be(1)
//      calcNumSteps(100, 115) should be(2)
//      calcNumSteps(100, 500) should be(5)
//      calcNumSteps(100, 900) should be(10)
//    }
//
//    it("should also work with step size") {
//      calcStepSize(100, 105, calcNumSteps(100, 105)) should be(5)
//      calcStepSize(100, 115, calcNumSteps(100, 115)) should be(7)
//      calcStepSize(100, 500, calcNumSteps(100, 500)) should be(80)
//      calcStepSize(100, 900, calcNumSteps(100, 900)) should be(80)
//    }
//
//    it("should work with step size of 1 (bug found)") {
//      val steps = calcNumSteps(870, 869)
//      steps should be(1)
//      calcStepSize(870, 869, steps) should be(-1)
//    }
//  }
//
//  describe("motion worker setup") {
//    val testStart = 0
//    val testDestination = 1000
//    val testDelay = 100
//
//    it("should be initialized properly") {
//      val props = MotionWorker.props(testStart, testDestination, testDelay, self, diagFlag = false)
//      val ms = TestActorRef[MotionWorker](props)
//      val under = ms.underlyingActor
//      under.start should equal(testStart)
//      under.destination should equal(testDestination)
//      under.delayInNanoSeconds should equal(testDelay * 1000000)
//    }
//  }
//
//  describe("motion worker forward") {
//    val testStart = 0
//    val testDestination = 1005
//    val testDelay = 10
//
//    it("should allow simulation on increasing encoder steps") {
//      val props = MotionWorker.props(testStart, testDestination, testDelay, self, diagFlag = false)
//      val ms = TestActorRef(props)
//      ms ! Start
//      val msgs = expectLLMoveMsgs()
//      msgs.last should be(End(testDestination))
//    }
//  }
//
//  describe("motion worker reverse") {
//    val testStart = 1000
//    val testDestination = -110
//    val testDelay = 10
//
//    it("should allow creation based on negative encoder steps") {
//      val props = MotionWorker.props(testStart, testDestination, testDelay, self, diagFlag = false)
//      val ms = TestActorRef(props)
//      ms ! Start
//      val msgs = expectLLMoveMsgs(false)
//      msgs.last should be(End(testDestination))
//    }
//  }
//
//  describe("simulate continuous motion with motion worker") {
//    val testStart = 500
//    val testDestination = 600
//    val testDelay = 10
//    it("should allow creation based on negative encoder steps") {
//
//      val props = MotionWorker.props(testStart, testDestination, testDelay, self, diagFlag = false)
//      val ms = TestActorRef(props)
//      ms ! Start
//      val msgs = expectLLMoveMsgs(false)
//      msgs.last should be(End(testDestination))
//    }
//  }
//
//  describe("motion worker cancel") {
//    val testStart = 0
//    val testDestination = 1000
//    val testDelay = 200
//
//    it("should allow cancelling after a few steps") {
//      val props = MotionWorker.props(testStart, testDestination, testDelay, self, diagFlag = false)
//      val ms = TestActorRef(props)
//      ms ! Start
//      expectMsg(Start)
//      // Wait 3 messages
//      receiveN(3, calcDelay(3, testDelay))
//      ms ! Cancel
//      // One more move
//      receiveN(1)
//      expectMsgClass(classOf[End])
//    }
//  }
//
//  val defaultAxisName = "test"
//  val defaultLowLimit = 100
//  val defaultLowUser = 200
//  val defaultHighUser = 1200
//  val defaultHighLimit = 1300
//  val defaultHome = 300
//  val defaultStartPosition = 350
//  val defaultStepDelayMS = 5
//  val defaultStatusPrefix = "test.axisStatus"
//
//  val defaultAxisConfig = AxisConfig(defaultAxisName,
//    defaultLowLimit,
//    defaultLowUser,
//    defaultHighUser,
//    defaultHighLimit,
//    defaultHome,
//    defaultStartPosition,
//    defaultStepDelayMS)
//
//  def defaultAxis(replyTo: ActorRef): TestActorRef[SingleAxisSimulator] = {
//    val props = SingleAxisSimulator.props(defaultAxisConfig, Some(replyTo))
//    TestActorRef(props) // No name here since can't create actors with the same name
//  }
//
//  describe("test single axis") {
//
//    it("should be creatable and initialize") {
//      val sa = defaultAxis(testActor)
//      sa.underlyingActor.axisConfig should equal(defaultAxisConfig)
//      // Expect an initial axis status message
//      // val one = expectMsgClass(1.second, classOf[AxisUpdate])
//      //one.current should equal(defaultAxisConfig.startPosition)
//    }
//
//    it("limitMove should clamp values") {
//      val ac = defaultAxisConfig
//
//      // Acceptable
//      limitMove(ac, 200) should equal(200)
//      // Low limit
//      limitMove(ac, 0) should equal(ac.lowLimit)
//
//      // High limit
//      limitMove(ac, 2000) should equal(ac.highLimit)
//
//      // Check "limit" checks > or < user limits
//      isHighLimit(ac, ac.home) should be(false)
//      isHighLimit(ac, ac.highUser - 1) should be(false)
//      isHighLimit(ac, ac.highUser) should be(true)
//      isHighLimit(ac, ac.highLimit) should be(true)
//
//      isLowLimit(ac, ac.home) should be(false)
//      isLowLimit(ac, ac.lowUser + 1) should be(false)
//      isLowLimit(ac, ac.lowUser) should be(true)
//      isLowLimit(ac, ac.lowLimit) should be(true)
//
//      isHomed(ac, ac.home)
//    }
//
//    it("Should init properly") {
//      val sa = defaultAxis(testActor)
//
//      // Expect an initial axis status message
//      //val one = expectMsgClass(classOf[AxisUpdate])
//      //one.current should equal(defaultAxisConfig.startPosition)
//
//      sa ! Datum
//      expectMsg(AxisStarted)
//      val upd = expectMsgClass(classOf[AxisUpdate])
//      upd.state should equal(AXIS_IDLE)
//      upd.current should equal(defaultAxisConfig.startPosition + 1)
//
//
//      sa ! GetStatistics
//      val stats1: AxisStatistics = expectMsgClass(classOf[AxisStatistics])
//      stats1.initCount should be(1)
//      stats1.moveCount should be(1)
//      stats1.homeCount should be(0)
//      stats1.limitCount should be(0)
//      stats1.successCount should be(1)
//      stats1.failureCount should be(0)
//      stats1.cancelCount should be(0)
//
//      sa ! PoisonPill
//    }
//
//    it("Should home properly") {
//      val sa = defaultAxis(testActor)
//
//      sa ! Home
//      val msgs = expectMoveMsgs()
//      msgs.last.state should be(AXIS_IDLE)
//      msgs.last.inHomed should be(true)
//      msgs.last.current should be(defaultAxisConfig.home)
//
//      sa.underlyingActor.current should be(defaultAxisConfig.home)
//
//      sa ! GetStatistics
//      val stats1: AxisStatistics = expectMsgClass(classOf[AxisStatistics])
//      stats1.initCount should be(0)
//      stats1.moveCount should be(1)
//      stats1.homeCount should be(1)
//      stats1.limitCount should be(0)
//      stats1.successCount should be(1)
//      stats1.failureCount should be(0)
//      stats1.cancelCount should be(0)
//
//      sa ! PoisonPill
//    }
//
//    it("Should move properly") {
//      val sa = defaultAxis(testActor)
//
//      // Expect an initial axis status message
//      //val one = expectMsgClass(classOf[AxisUpdate])
//      //one.current should equal(defaultAxisConfig.startPosition)
//
//      sa ! Move(500, diagFlag = false)
//      val msgs = expectMoveMsgs()
//      msgs.last.state should be(AXIS_IDLE)
//      msgs.last.current should be(500)
//
//      sa.underlyingActor.current should be(500)
//
//      sa ! PoisonPill
//    }
//
//    it("Should move and update") {
//      val sa = defaultAxis(testActor)
//
//      // Sleeps are to try and not do all the updates up front before movement starts
//      sa ! Move(360)
//      Thread.sleep(30)
//      sa ! Move(365)
//      Thread.sleep(20)
//      sa ! Move(390)
//      Thread.sleep(30)
//      sa ! Move(420)
//      Thread.sleep(20)
//      sa ! Move(425)
//
//      val msgs = expectMoveMsgsWithDest(425)
//      msgs.last.isInstanceOf[AxisUpdate]
//      val last:AxisUpdate = msgs.last.asInstanceOf[AxisUpdate]
//      last.state should be(AXIS_IDLE)
//      last.current should be(425)
//
//      sa ! PoisonPill
//    }
//
//    it("Should allow a cancel") {
//      val sa = defaultAxis(testActor)
//
//      // Expect an initial axis status message
//      //val one = expectMsgClass(classOf[AxisUpdate])
//      //one.current should equal(defaultAxisConfig.startPosition)
//
//      sa ! Move(850, diagFlag = false)
//      expectMsg(AxisStarted)
//      // Wait 2 updates
//      receiveN(2)
//      sa ! CancelMove
//      // One more update due to algo
//      val lastmsg = receiveN(1)
//      val end = expectMsgClass(classOf[AxisUpdate])
//      end.state should be(AXIS_IDLE)
//      end.current should be(650)
//
//      sa ! PoisonPill
//    }
//
//    it("should limit out-of-range moves") {
//      val sa = defaultAxis(testActor)
//
//      // Expect an initial axis status message
//      //val one = expectMsgClass(classOf[AxisUpdate])
//      //one.current should equal(defaultAxisConfig.startPosition)
//
//      // Position starts out at 350
//      sa ! Move(0)
//      val msgs = expectMoveMsgs(false)
//      msgs.last.state should be(AXIS_IDLE)
//      msgs.last.current should be(100)
//      msgs.last.inLowLimit should be(true)
//
//      sa.underlyingActor.current should be(defaultAxisConfig.lowLimit)
//      sa.underlyingActor.inLowLimit should be(true)
//      sa.underlyingActor.inHighLimit should be(false)
//
//      sa ! GetStatistics
//      val stats1: AxisStatistics = expectMsgClass(classOf[AxisStatistics])
//      stats1.initCount should be(0)
//      stats1.moveCount should be(1)
//      stats1.homeCount should be(0)
//      stats1.limitCount should be(1)
//      stats1.successCount should be(1)
//      stats1.failureCount should be(0)
//      stats1.cancelCount should be(0)
//
//      sa ! Move(2000)
//      val msgs2 = expectMoveMsgs(false)
//      msgs2.last.state should be(AXIS_IDLE)
//      msgs2.last.current should be(1300)
//      msgs2.last.inLowLimit should be(false)
//      msgs2.last.inHighLimit should be(true)
//
//      sa.underlyingActor.current should be(defaultAxisConfig.highLimit)
//      sa.underlyingActor.inLowLimit should be(false)
//      sa.underlyingActor.inHighLimit should be(true)
//
//      sa ! GetStatistics
//      val stats2 = expectMsgClass(classOf[AxisStatistics])
//      stats2.initCount should be(0)
//      stats2.moveCount should be(2)
//      stats2.homeCount should be(0)
//      stats2.limitCount should be(2)
//      stats2.successCount should be(2)
//      stats2.failureCount should be(0)
//      stats2.cancelCount should be(0)
//
//      sa ! PoisonPill
//    }
//
//    it("should support a complex example") {
//      val sa = defaultAxis(testActor)
//
//      // Expect an initial axis status message
//      //val one = expectMsgClass(classOf[AxisUpdate])
//      //one.current should equal(defaultAxisConfig.startPosition)
//
//      // Starts at 350, init (351), go home, go to 423, 800, 560, highlmit at 1240, then home
//      sa ! Datum
//      var msgs = expectMoveMsgs()
//      msgs.last.current should be(defaultAxisConfig.startPosition + 1)
//
//      sa ! Home
//      msgs = expectMoveMsgs()
//      msgs.last.current should be(defaultAxisConfig.home)
//
//      sa ! Move(423)
//      msgs = expectMoveMsgs()
//      msgs.last.current should be(423)
//
//      sa ! Move(800)
//      msgs = expectMoveMsgs()
//      msgs.last.current should be(800)
//
//      sa ! Move(560)
//      msgs = expectMoveMsgs()
//      msgs.last.current should be(560)
//
//      sa ! Move(1240)
//      msgs = expectMoveMsgs()
//      msgs.last.current should be(1240)
//
//      sa ! Home
//      msgs = expectMoveMsgs()
//      msgs.last.current should be(defaultAxisConfig.home)
//
//      sa ! GetStatistics
//      val stats2 = expectMsgClass(classOf[AxisStatistics])
//      stats2.initCount should be(1)
//      stats2.moveCount should be(7)
//      stats2.homeCount should be(2)
//      stats2.limitCount should be(1)
//      stats2.successCount should be(7)
//      stats2.failureCount should be(0)
//      stats2.cancelCount should be(0)
//
//      sa ! PoisonPill
//    }
//  }
//}