package wfos.grxassembly

//import akka.actor.Status.Success
import csw.command.client.CommandServiceFactory
import csw.location.api.models.Connection.AkkaConnection
import csw.prefix.models.Prefix
import csw.location.api.models.{ComponentId, ComponentType}

import csw.params.commands.Setup
//import csw.prefix.models.Subsystem.WFOS
import csw.testkit.scaladsl.CSWService.{AlarmServer, EventServer}
import csw.testkit.scaladsl.ScalaTestFrameworkTestKit
import org.scalatest.funsuite.AnyFunSuiteLike

import scala.concurrent.Await
import scala.concurrent.duration._

class GrxassemblyTest extends ScalaTestFrameworkTestKit(AlarmServer, EventServer) with AnyFunSuiteLike {

  import frameworkTestKit._

  override def beforeAll(): Unit = {
    super.beforeAll()
    // uncomment if you want one Assembly run for all tests
    spawnStandalone(com.typesafe.config.ConfigFactory.load("GrxassemblyStandalone.conf"))
  }

  test("Assembly should be locatable using Location Service") {
    val connection = AkkaConnection(ComponentId(Prefix("wfos.grxAssembly"), ComponentType.Assembly))
    Await.result(locationService.resolve(connection, 10.seconds), 10.seconds) match {
      case None =>
        println("Assembly connection not found")
      case Some(loc) =>
        val assembly = CommandServiceFactory.make(loc)
        println("Connection found")
      //        assembly.submitAndWait(makeSetup()).onComplete
//      case Success(response) =>
//        println(s"single submit test passed")
//      case Failure(reason) =>
//        println(s"Single submit test failed")

    }
//      val commandService = CommandServiceFactory.make(akkaLocation)
//    val setup = Setup(sequencer)
//    val validate =
//      commandService.validate(setup).futurevalue
  }
}
