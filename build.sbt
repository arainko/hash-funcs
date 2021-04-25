val zioVersion = "1.0.6"

lazy val `hash-funcs` = (project in file("."))
  .settings(
    name := "block",
    version := "1.0.0",
    scalaVersion := "2.13.4",
    scalacOptions ++= Seq("-Wconf:cat=unused:info", "-Ymacro-annotations"),
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    libraryDependencies ++= Seq(
      "org.scodec"    %% "scodec-core"       % "1.11.7",
      "org.scodec"    %% "scodec-bits"       % "1.1.24",
      "org.typelevel" %% "cats-core"         % "2.4.2",
      "dev.zio"       %% "zio-nio"           % "1.0.0-RC10",
      "dev.zio"       %% "zio"               % zioVersion,
      "dev.zio"       %% "zio-test"          % zioVersion % "test",
      "dev.zio"       %% "zio-test-sbt"      % zioVersion % "test",
      "dev.zio"       %% "zio-test-magnolia" % zioVersion % "test"
    )
  )
  .configs(IntegrationTest)
  .settings(Defaults.itSettings)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

scalafixScalaBinaryVersion in ThisBuild := CrossVersion.binaryScalaVersion(scalaVersion.value)
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.5.0"
