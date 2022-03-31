package wfos.grxassembly.command

import akka.http.scaladsl.model.StatusCodes.Accepted
import wfos.grxassembly.models.{BluegrxPosition, GripperPosition}
import csw.params.commands.CommandIssue.{MissingKeyIssue, ParameterValueOutOfRangeIssue}
import csw.params.commands.CommandResponse.ValidateCommandResponse
import csw.params.commands.{CommandIssue, CommandName, Setup}
import csw.params.core.generics.{GChoiceKey, Key, KeyType, Parameter}
import csw.params.core.models.{Id, Units}

abstract class SelectCommand {
  val Name: CommandName = CommandName("SELECT")
//  val gripper1Key: GChoiceKey
  def Validate(setup: Setup): Either[CommandIssue, Parameter[String]]
}

object BlueSelectCommand extends SelectCommand {

  val bgidKey: Key[String]        = KeyType.StringKey.make("bgId")
  val GratingModeKey: Key[String] = KeyType.StringKey.make("GratingModes")
  val targetAngleKey: Key[Double] = KeyType.DoubleKey.make("TargetAngle")

  val degreeKey: Key[Double]                       = KeyType.DoubleKey.make("degree")
  def setdegree(degree: Double): Parameter[Double] = cwKey.set(degree).withUnits(Units.degree)
  val lowdegreeKey                                 = setdegree(0)
  val highdegreeKey                                = setdegree(55)

  val cwKey: Key[Double]                        = KeyType.DoubleKey.make("commonWavelength")
  def setcwLimit(cw: Double): Parameter[Double] = cwKey.set(cw).withUnits(Units.angstrom)
  val lowLimitKey                               = setcwLimit(3100)
  val highLimitKey                              = setcwLimit(9000)

//  override val gripper1Key: GChoiceKey = BluegrxPosition.makeChoiceKey("gripper1")

  override def Validate(setup: Setup): Either[CommandIssue, Parameter[String]] = {
    val issueOraccepted = for {
      bgid        <- setup.get(bgidKey).toRight(CommandIssue.WrongParameterTypeIssue("bgid not found"))
      targetAngle <- setup.get(targetAngleKey).toRight(CommandIssue.WrongParameterTypeIssue("targetAngle not found"))
      cw          <- setup.get(cwKey).toRight(CommandIssue.WrongParameterTypeIssue("CW not found"))
      gratingmode <- setup.get(GratingModeKey).toRight(CommandIssue.WrongParameterTypeIssue("GratingMode not found"))
      _           <- inRange(cw, lowLimitKey, highLimitKey)
      _           <- inRange(targetAngle, lowdegreeKey, highdegreeKey)
      param       <- validateParam(bgid, targetAngle)
    } yield param
    issueOraccepted
  }

  private def inRange(parameter: Parameter[Double], minVal: Parameter[Double], maxVal: Parameter[Double]) = {
    if (parameter.head >= minVal.head & parameter.head <= maxVal.head) Right(parameter)
    else Left(CommandIssue.WrongParameterTypeIssue(s"${parameter.keyName} should be in range of $minVal and $maxVal"))
  }

  private def validateParam(bgid: Parameter[String], targetAngle: Parameter[Double]) = {
    bgid.head match {
      case "bgid1" =>
        if (targetAngle.values.head == 0) Right(bgid)
        else Left(CommandIssue.ParameterValueOutOfRangeIssue(s"targetAngle should be in range"))
      case "bgid2" =>
        if (targetAngle.values.head == 15) Right(bgid)
        else Left(CommandIssue.ParameterValueOutOfRangeIssue(s"targetAngle should be in range"))
      case "bgid3" =>
        if (targetAngle.values.head >= 25 & targetAngle.values.head <= 35) Right(bgid)
        else Left(CommandIssue.ParameterValueOutOfRangeIssue(s"targetAngle should be in range"))
      case "bgid4" =>
        if (targetAngle.values.head == 45) Right(bgid)
        else Left(CommandIssue.ParameterValueOutOfRangeIssue(s"targetAngle should be in range"))
      case "bgid5" =>
        if (targetAngle.values.head == 45) Right(bgid)
        else Left(CommandIssue.ParameterValueOutOfRangeIssue(s"targetAngle should be in range"))
      case id => Left(CommandIssue.WrongParameterTypeIssue(s"Wrong Bgid"))
    }
  }
}
