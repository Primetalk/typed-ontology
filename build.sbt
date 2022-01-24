val scala3Version = "3.1.0"
val mainVersion = "0.1.0-SNAPSHOT"

ThisBuild / organization := "ru.primetalk"
ThisBuild / version      := mainVersion
ThisBuild / scalaVersion := scala3Version

// scalacOptions ++= Seq(
//    "-deprecation",         // emit warning and location for usages of deprecated APIs
//    "-explain",             // explain errors in more detail
//    "-explain-types",       // explain type errors in more detail
//    "-feature",             // emit warning and location for usages of features that should be imported explicitly
//    "-indent",              // allow significant indentation.
//    "-new-syntax",          // require `then` and `do` in control expressions.
//    "-print-lines",         // show source code line numbers.
//    "-unchecked",           // enable additional warnings where generated code depends on assumptions
//    "-Ykind-projector",     // allow `*` as wildcard to be compatible with kind projector
//    "-Xfatal-warnings",     // fail the compilation if there are any warnings
//    "-Xmigration"           // warn about constructs whose behavior may have changed since version
//   //  "-Xmax-inlines=64"
// )

val catsEffect = "org.typelevel" %% "cats-effect" % "3.3.4"
val catsCore   = "org.typelevel" %% "cats-core" % "2.7.0"

val commonSettings = Seq(
  scalaVersion := scala3Version,
  // scalacOptions += "-Xmax-inlines=50",
  libraryDependencies ++= Seq(
    catsCore,
    "com.novocode" % "junit-interface" % "0.11" % Test,
    "org.scalacheck" %% "scalacheck" % "1.15.4" % Test,
    "org.scalatest"  %% "scalatest"  % "3.2.10" % Test,
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
  )
  .settings(commonSettings :_*)

lazy val typedOntologyMeta = project
  .in(file("typed-ontology-meta"))
  .settings(
    name := "typed-ontology-meta",
  )
  .dependsOn(typedOntologyMetaMeta)
  .settings(commonSettings :_*)

lazy val ontologyExample1 = project
  .in(file("ontology-example1"))
  .settings(
    name := "ontology-example1",
  )
  .dependsOn(
    typedOntologyMeta
  )
  .settings(commonSettings :_*)
