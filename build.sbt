val scala3Version = "3.1.0"
val mainVersion = "0.1.0-SNAPSHOT"

ThisBuild / organization := "ru.primetalk"
ThisBuild / version      := mainVersion
ThisBuild / scalaVersion := scala3Version

val commonSettings = Seq(
  scalaVersion := scala3Version,
  libraryDependencies ++= Seq(
    "com.novocode" % "junit-interface" % "0.11" % "test",
  )
)

lazy val root = (project in file("."))
  .aggregate(
    typedOntologyMetaMeta,
    typedOntologyMeta,
    ontologyExample1,
  )
  .settings(
    name := "typed-ontology"
  )

lazy val typedOntologyMetaMeta = project
  .in(file("typed-ontology-metameta"))
  .settings(
    name := "typed-ontology-metameta",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.4"
  )
  .settings(commonSettings :_*)

lazy val typedOntologyMeta = project
  .in(file("typed-ontology-meta"))
  .settings(
    name := "typed-ontology-meta",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.4"
  )
  .dependsOn(typedOntologyMetaMeta)
  .settings(commonSettings :_*)

lazy val ontologyExample1 = project
  .in(file("ontology-example1"))
  .settings(
    name := "ontology-example1",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.4"
  )
  .dependsOn(
    typedOntologyMeta
  )
  .settings(commonSettings :_*)
