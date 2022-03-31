package wfos.wfosicsdeploy

import csw.framework.deploy.containercmd.ContainerCmd
import csw.prefix.models.Subsystem

object WfosIcsContainerCmdApp extends App {

  ContainerCmd.start("wfos_ics_container_cmd_app", Subsystem.withNameInsensitive("wfos"), args)

}
