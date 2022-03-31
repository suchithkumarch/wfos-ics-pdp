package wfos.grxassembly

import akka.actor.typed.{ActorRef, Scheduler}
import akka.actor.typed.scaladsl.ActorContext
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
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
import wfos.grxassembly.models.AssemblyConfiguration
import wfos.grxassembly.models.GripperCommand
import wfos.grxassembly.models.GripperCommand.IsValidMove
import wfos.grxassembly.models.GripperPosition

import wfos.grxassembly.command.{SelectCommand, BlueSelectCommand}

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, ExecutionContext}

/**
 * Domain specific logic should be written in below handlers.
 * This handlers gets invoked when component receives messages/commands from other component/entity.
 * For example, if one component sends Submit(Setup(args)) command to Linearhcd,
 * This will be first validated in the supervisor and then forwarded to Component TLA which first invokes validateCommand hook
 * and if validation is successful, then onSubmit hook gets invoked.
 * You can find more information on this here : https://tmtsoftware.github.io/csw/commons/framework.html
 */
class GrxassemblyHandlers(ctx: ActorContext[TopLevelActorMessage], cswCtx: CswContext) extends ComponentHandlers(ctx, cswCtx) {

  import cswCtx._
  implicit val a: Scheduler = ctx.system.scheduler

  implicit val ec: ExecutionContext = ctx.executionContext
  private val log                   = loggerFactory.getLogger
  private val prefix: Prefix        = cswCtx.componentInfo.prefix

  //  val initialPosition: GripperPosition

  val selectCommand: SelectCommand = BlueSelectCommand

  override def initialize(): Unit = {
    log.info(s"Assembly: $prefix initialize")
  }

  override def onLocationTrackingEvent(trackingEvent: TrackingEvent): Unit = {}

  override def validateCommand(runId: Id, controlCommand: ControlCommand): ValidateCommandResponse = {
    val timeout: FiniteDuration = 1.seconds
    implicit val value: Timeout = Timeout(timeout)

    val validateParamsRes = controlCommand match {
      case cmd: Setup => validateSetupParams(runId, cmd)
      case observe    => Invalid(runId, UnsupportedCommandIssue(s"$observe command not supported."))
    }
    validateParamsRes

//    validateParamsRes match {
//      case _: Accepted => Await.result(filterActor ? (IsValidMove(runId, _)), timeout)
//      case invalidRes  => invalidRes
//    }
  }
  private def validateSelectParams(runId: Id, setup: Setup): ValidateCommandResponse =
    selectCommand.Validate(setup) match {
      case Right(_) => Accepted(runId)
      case Left(commandIssue: UnsupportedCommandIssue) =>
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

  override def onSubmit(runId: Id, controlCommand: ControlCommand): SubmitResponse = Completed(runId)

  override def onOneway(runId: Id, controlCommand: ControlCommand): Unit = {}

  override def onShutdown(): Unit = {
    log.info(s"Assembly: $prefix is shutting down")
  }

  override def onGoOffline(): Unit = {}

  override def onGoOnline(): Unit = {}

  override def onDiagnosticMode(startTime: UTCTime, hint: String): Unit = {}

  override def onOperationsMode(): Unit = {}

}
