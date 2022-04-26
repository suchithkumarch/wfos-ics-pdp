package wfos.grxassembly

import akka.actor.typed.Scheduler
import akka.actor.typed.scaladsl.ActorContext
//import akka.util.Timeout
//import com.typesafe.config.ConfigFactory
import csw.command.client.messages.TopLevelActorMessage
import csw.framework.models.CswContext
import csw.framework.scaladsl.ComponentHandlers
import csw.location.api.models.TrackingEvent
import csw.params.commands.CommandIssue.UnsupportedCommandIssue
import csw.params.commands.CommandResponse._
import csw.params.commands.{CommandName, ControlCommand, Setup}
import csw.params.core.models.Id
import csw.prefix.models.Prefix
import csw.time.core.models.UTCTime
import wfos.grxassembly.models.{BluegrxPosition, GripperPosition}
import wfos.grxassembly.command.{BlueSelectCommand, SelectCommand}
import wfos.grxassembly.events.{BlueGrxPositionEvent, GrxPositionEvent}

//import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.ExecutionContext

class GrxassemblyHandlers(ctx: ActorContext[TopLevelActorMessage], cswCtx: CswContext) extends ComponentHandlers(ctx, cswCtx) {

  import cswCtx._
  implicit val a: Scheduler = ctx.system.scheduler

  implicit val ec: ExecutionContext = ctx.executionContext
  private val log                   = loggerFactory.getLogger
  private val prefix: Prefix        = cswCtx.componentInfo.prefix

  val initialPosition: GripperPosition      = BluegrxPosition.left_edge
  val filterPositionEvent: GrxPositionEvent = new BlueGrxPositionEvent(prefix)

  val selectCommand: SelectCommand = BlueSelectCommand

  override def initialize(): Unit = {
    log.info(s"Assembly: $prefix initialize")
  }

  override def onLocationTrackingEvent(trackingEvent: TrackingEvent): Unit = {}

  override def validateCommand(runId: Id, controlCommand: ControlCommand): ValidateCommandResponse = {
    val validateParamsRes = controlCommand match {
      case cmd: Setup => validateSetupParams(runId, cmd)
      case observe    => Invalid(runId, UnsupportedCommandIssue(s"$observe command not supported."))
    }
    validateParamsRes
  }

  private def validateSelectParams(runId: Id, setup: Setup): ValidateCommandResponse =
    selectCommand.Validate(setup) match {
      case Right(_) => Accepted(runId)
      case Left(commandIssue) =>
        log.error(s"grx Assembly: Failed to validate, reason ${commandIssue.reason}")
        Invalid(runId, UnsupportedCommandIssue(s"Validation Failed"))
    }

  private def validateSetupParams(runId: Id, setup: Setup): ValidateCommandResponse = setup.commandName match {
    case selectCommand.Name => validateSelectParams(runId, setup)
    case CommandName(name) =>
      val errMsg = s"grx Assembly: Setup command: $name not supported."
      log.error(errMsg)
      Invalid(runId, UnsupportedCommandIssue(errMsg))
  }

  override def onSubmit(runId: Id, controlCommand: ControlCommand): SubmitResponse = {
    controlCommand match {
      case setup: Setup => handleSelect(runId, setup)
      case observe      => Invalid(runId, UnsupportedCommandIssue(s"$observe commmand not supported."))
    }
  }

  def handleSelect(runId: Id, setup: Setup): SubmitResponse = {
    selectCommand.Validate(setup) match {
      case Right(targetAngle) =>
        log.info(s"Gripper Assembly: Rotating gripper to target angle: $targetAngle")
//        filterActor ! GripperCommand.Move(BluegrxPosition.right_edge, runId)
        Started(runId)
      case Left(commandIssue) =>
        log.error(s"Gripper Assembly: Failed to retrieve target Angle, reason: ${commandIssue.reason}")
        Invalid(runId, commandIssue)
    }
  }

  override def onOneway(runId: Id, controlCommand: ControlCommand): Unit = {}

  override def onShutdown(): Unit = {
    log.info(s"Assembly: $prefix is shutting down")
  }

  override def onGoOffline(): Unit = {}

  override def onGoOnline(): Unit = {}

  override def onDiagnosticMode(startTime: UTCTime, hint: String): Unit = {}

  override def onOperationsMode(): Unit = {}

}
