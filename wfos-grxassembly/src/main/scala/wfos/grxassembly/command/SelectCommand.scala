package wfos.grxassembly.command

import wfos.grxassembly.models.BluegrxPosition
import csw.params.commands.{CommandIssue, CommandName, Setup}
import csw.params.core.generics.{GChoiceKey, Key, KeyType, Parameter}
import csw.params.core.models.Choice

abstract class SelectCommand {
  val Name: CommandName = CommandName("SELECT")
  val gripperKey: GChoiceKey
  def Validate(setup: Setup): Either[CommandIssue, Parameter[Double]]
}

object BlueSelectCommand extends SelectCommand {

  val gripperKey: GChoiceKey      = BluegrxPosition.makeChoiceKey("BgidKey")
  val GratingModeKey: Key[String] = KeyType.StringKey.make("GratingModes")
  val targetAngleKey: Key[Double] = KeyType.DoubleKey.make("TargetAngle")
  val degreeKey: Key[Double]      = KeyType.DoubleKey.make("degree")
  val cwKey: Key[Double]          = KeyType.DoubleKey.make("commonWavelength")

  val lowdegreeKey  = 0
  val highdegreeKey = 55

  val lowLimitKey  = 3100
  val highLimitKey = 9000

  override def Validate(setup: Setup): Either[CommandIssue, Parameter[Double]] = {
    val issueOraccepted = for {
      bgid        <- setup.get(gripperKey).toRight(CommandIssue.WrongParameterTypeIssue("bgid not found"))
      targetAngle <- setup.get(targetAngleKey).toRight(CommandIssue.WrongParameterTypeIssue("targetAngle not found"))
      cw          <- setup.get(cwKey).toRight(CommandIssue.WrongParameterTypeIssue("CW not found"))
      _           <- setup.get(GratingModeKey).toRight(CommandIssue.WrongParameterTypeIssue("GratingMode not found"))
      _           <- inRange(cw, lowLimitKey, highLimitKey)
      _           <- inRange(targetAngle, lowdegreeKey, highdegreeKey)
      param       <- validateParam(bgid, targetAngle)
    } yield param
    issueOraccepted
  }

  private def inRange(parameter: Parameter[Double], minVal: Double, maxVal: Double) = {
    if (parameter.head >= minVal & parameter.head <= maxVal) Right(parameter)
    else Left(CommandIssue.WrongParameterTypeIssue(s"${parameter.keyName} should be in range of $minVal and $maxVal"))
  }

  private def validateParam(bgid: Parameter[Choice], targetAngle: Parameter[Double]) = {
    bgid.keyName match {
      case "bgid1" =>
        if (targetAngle.values.head == 0) Right(targetAngle)
        else Left(CommandIssue.ParameterValueOutOfRangeIssue(s"targetAngle should be in range"))
      case "bgid2" =>
        if (targetAngle.values.head == 15) Right(targetAngle)
        else Left(CommandIssue.ParameterValueOutOfRangeIssue(s"targetAngle should be in range"))
      case "bgid3" =>
        if (targetAngle.values.head >= 25 & targetAngle.values.head <= 35) Right(targetAngle)
        else Left(CommandIssue.ParameterValueOutOfRangeIssue(s"targetAngle should be in range"))
      case "bgid4" =>
        if (targetAngle.values.head == 45) Right(targetAngle)
        else Left(CommandIssue.ParameterValueOutOfRangeIssue(s"targetAngle should be in range"))
      case "bgid5" =>
        if (targetAngle.values.head == 45) Right(targetAngle)
        else Left(CommandIssue.ParameterValueOutOfRangeIssue(s"targetAngle should be in range"))
      case _ => Left(CommandIssue.WrongParameterTypeIssue(s"Wrong Bgid"))
    }
  }
}
