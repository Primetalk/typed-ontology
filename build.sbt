val scala3Version = "3.1.2-RC2"
val mainVersion = "0.2.2-SNAPSHOT"

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
val catsCore   = "org.typelevel" %% "cats-core"   % "2.7.0"
val fs2 = libraryDependencies ++= Seq(
  "co.fs2" %% "fs2-core" % "3.2.4",
  "co.fs2" %% "fs2-io"   % "3.2.4",
)

val commonSettings = Seq(
  scalaVersion := scala3Version,
  // scalacOptions += "-Xmax-inlines=50",
  libraryDependencies ++= Seq(
    catsCore,
    "com.novocode" % "junit-interface" % "0.11" % Test,
    "org.scalacheck" %% "scalacheck" % "1.15.4" % Test,
    "org.scalatest"  %% "scalatest"  % "3.2.11" % Test,
  )
)

lazy val root = (project in file("."))
  .aggregate(
    typedOntologyMetaMeta,
    typedOntologyMeta,
    typedOntologySimpleMeta,
    sourceCode,
    ontologyExample1,
  )
  .settings(
    name := "typed-ontology"
  )

lazy val sourceCode = project
  .in(file("source-code"))
  .settings(
    name := "source-code",
  )
  .settings(commonSettings :_*)

lazy val typedOntologyMetaMeta = project
  .in(file("typed-ontology-metameta"))
  .settings(
    name := "typed-ontology-metameta",
    fs2,
  )
  .settings(commonSettings :_*)

lazy val typedOntologyMeta = project
  .in(file("typed-ontology-meta"))
  .settings(
    name := "typed-ontology-meta",
  )
  .dependsOn(typedOntologyMetaMeta)
  .settings(commonSettings :_*)

lazy val typedOntologySimpleMeta = project
  .in(file("typed-ontology-simple-meta"))
  .settings(
    name := "typed-ontology-simple-meta",
  )
  .dependsOn(typedOntologyMetaMeta)
  .settings(commonSettings :_*)

lazy val ontologyExample1 = project
  .in(file("ontology-example1"))
  .settings(
    name := "ontology-example1",
  )
  .dependsOn(
    typedOntologySimpleMeta
  )
  .settings(commonSettings :_*)
