package org.tmt.csw.cmd.akka

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.Future
import akka.actor.{ActorRef, Props}

object TestConfigActorWorker {
  def props(commandStatusActor: ActorRef, numberOfSecondsToRun: Int): Props =
    Props(classOf[TestConfigActorWorker], commandStatusActor, numberOfSecondsToRun)
}

/**
 * A test config worker actor.
 *
 * @param commandStatusActor actor that receives the command status messages
 * @param numberOfSecondsToRun the number of seconds to run the simulated work
 */
class TestConfigActorWorker(override val commandStatusActor: ActorRef, numberOfSecondsToRun: Int) extends ConfigActor {
  import CommandQueueActor._

  // Receive config messages
  override def receive: Receive = receiveConfigs

  // This is just a worker and doesn't need to register any config paths
  override val configPaths = Set.empty[String]


  // Used as an example of one way to implement interrupting a running config
  val aState: AtomicReference[CommandStatus] = new AtomicReference(null)

  // Saved position and config for Pause, so that we can continue on Resume
  var savedPos = 1

  // Needed to implement the "resume" message
  var savedSubmit: SubmitWithRunId = null

  /**
   * Called when a configuration is submitted
   */
  def submit(submit: SubmitWithRunId): Unit = {
    savedSubmit = submit
    aState.set(CommandStatus.Submitted(submit.runId))
    doSubmit(submit)
  }

  def doSubmit(submit: SubmitWithRunId): Unit = {
    log.info(s"Processing config: ${submit.config}, reply when complete to ${submit.submitter}")
    implicit val dispatcher = context.system.dispatcher
    for {
      status <- Future {
        doWork(submit)
      } recover {
        case ex: Exception => CommandStatus.Error(submit.runId, ex.getMessage)
      }
    } {
      if (status != CommandStatus.Paused(submit.runId)) {
        commandStatusActor ! CommandStatusActor.StatusUpdate(status, submit.submitter)
        if (context != null) context.system.stop(self)
      }
    }
  }

  // Do some work (sleeping in a loop), and check for state changes
  def doWork(submit: SubmitWithRunId): CommandStatus = {
    for (a <- savedPos to numberOfSecondsToRun) {
      // Check if we should stop
      val state = aState.get
      if (state.stop) {
        if (state == CommandStatus.Paused(submit.runId)) savePos(a)
        return state // Return the state to the sender
      } else {
        // Continue working
        log.info(s"${self.path} busy working on part $a of $numberOfSecondsToRun")
        Thread.sleep(1000) // do some work...
      }
    }
    // Send the config state back to the original sender
    aState.get() match {
      case CommandStatus.Submitted(runId) => CommandStatus.Completed(submit.runId)
      case CommandStatus.Resumed(runId) => CommandStatus.Completed(submit.runId)
      case other => other // may have been aborted or canceled
    }
  }

  // Save the current position so we can resume processing later (when resume is called)
  def savePos(a: Int): Unit = {
    savedPos = a
  }

  /**
   * Work on the config matching the given runId should be paused
   */
  def pause(runId: RunId): Unit = {
    aState.set(CommandStatus.Paused(runId))
  }

  /**
   * Work on the config matching the given runId should be resumed
   */
  def resume(runId: RunId): Unit = {
    aState.set(CommandStatus.Resumed(runId))
    doSubmit(savedSubmit)
  }

  /**
   * Work on the config matching the given runId should be canceled
   */
  def cancel(runId: RunId): Unit = {
    aState.set(CommandStatus.Canceled(runId))
  }

  /**
   * Work on the config matching the given runId should be aborted
   */
  def abort(runId: RunId): Unit = {
    aState.set(CommandStatus.Aborted(runId))
  }
}
