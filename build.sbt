
lazy val aggregatedProjects: Seq[ProjectReference] = Seq(
  `wfos-grxassembly`,
  `wfos-linearhcd`,
  `wfos-wfos-icsdeploy`
)

lazy val `wfos-ics-root` = project
  .in(file("."))
  .aggregate(aggregatedProjects: _*)

// assembly module
lazy val `wfos-grxassembly` = project
  .settings(
    libraryDependencies ++= Dependencies.Grxassembly
  )

// hcd module
lazy val `wfos-linearhcd` = project
  .settings(
    libraryDependencies ++= Dependencies.Linearhcd
  )

// deploy module
lazy val `wfos-wfos-icsdeploy` = project
  .dependsOn(
    `wfos-grxassembly`,
    `wfos-linearhcd`
  )
  .settings(
    libraryDependencies ++= Dependencies.WfosIcsDeploy
  )
