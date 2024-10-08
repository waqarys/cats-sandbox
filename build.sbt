name := "cats-sandbox"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.13.14"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.9"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"
libraryDependencies += "org.typelevel" %% "discipline-core" % "1.5.0"
libraryDependencies ++= Seq(
  "org.typelevel" %% "discipline-scalatest" % "2.3.0",
  "org.typelevel" %% "cats-laws" % "2.0.0",
)

// scalac options come from the sbt-tpolecat plugin so need to set any here

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.3" cross CrossVersion.full)
