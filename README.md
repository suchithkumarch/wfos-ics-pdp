# wfos_ics

This project implements an HCD (Hardware Control Daemon) and an Assembly using
TMT Common Software ([CSW](https://github.com/tmtsoftware/csw)) APIs.

## Subprojects

* wfos-grxassembly - an assembly that talks to the wfos_ics HCD
* wfos-linearhcd - an HCD that talks to the wfos_ics hardware
* wfos-wfos-icsdeploy - for starting/deploying HCDs and assemblies

## Upgrading CSW Version

`project/build.properties` file contains `csw.version` property which indicates CSW version number.
Updating `csw.version` property will make sure that CSW services as well as library dependency for HCD and Assembly modules are using same CSW version.

## Build Instructions

The build is based on sbt and depends on libraries generated from the
[csw](https://github.com/tmtsoftware/csw) project.

See [here](https://www.scala-sbt.org/1.0/docs/Setup.html) for instructions on installing sbt.

## Prerequisites for running Components

The CSW services need to be running before starting the components.
   This is done by starting the `csw-services`.
   If you are not building csw from the sources, you can run `csw-services` as follows:

- Install `coursier` using steps described [here](https://tmtsoftware.github.io/csw/apps/csinstallation.html) and add TMT channel.
- Run `cs install csw-services`. This will create an executable file named `csw-services` in the default installation directory.
- Run `csw-services start` command to start all the CSW services i.e. Location, Config, Event, Alarm and Database Service
- Run `csw-services --help` to get more information.

Note: while running the csw-services use the csw version from `project/build.properties`

## Running the HCD and Assembly

Run the container cmd script with arguments. For example:

* Run the HCD in a standalone mode with a local config file (The standalone config format is different than the container format):

```
sbt "wfos-wfos-icsdeploy/runMain wfos.wfosicsdeploy.WfosIcsContainerCmdApp --standalone --local ./src/main/resources/LinearhcdStandalone.conf"
```

* Start the HCD and assembly in a container using the Java implementations:

```
sbt "wfos-wfos-icsdeploy/runMain wfos.wfosicsdeploy.WfosIcsContainerCmdApp --local ./src/main/resources/JWfosIcsContainer.conf"
```
