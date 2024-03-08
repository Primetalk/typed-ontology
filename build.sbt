val scala3Version = "3.4.1-RC1"
val mainVersion   = "0.2.5"

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / organization  := "ru.primetalk"
ThisBuild / version       := mainVersion
ThisBuild / scalaVersion  := scala3Version
ThisBuild / versionScheme := Some("early-semver")

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

val catsEffect = "org.typelevel" %% "cats-effect" % "3.5.0"
val catsCore   = "org.typelevel" %% "cats-core"   % "2.9.0"
val fs2 = libraryDependencies ++= Seq(
  "co.fs2" %% "fs2-core" % "3.7.0",
  "co.fs2" %% "fs2-io"   % "3.7.0"
)

val commonSettings = Seq(
  scalaVersion := scala3Version,
  scalacOptions ++= Seq(
    // "-Xmax-inlines=50",
    "-deprecation"
  ),
  libraryDependencies ++= Seq(
    catsCore,
    "io.github.kitlangton" %% "quotidian"       % "0.0.10",
    "com.novocode"          % "junit-interface" % "0.11"   % Test,
    "org.scalacheck"       %% "scalacheck"      % "1.17.0" % Test,
    "org.scalatest"        %% "scalatest"       % "3.2.15" % Test
  )
)

lazy val root = (project in file("."))
  .aggregate(
    typedOntologyMetaMeta,
    typedOntologyMeta,
    typedOntologySimpleMeta,
    ontologyExample1
  )
  .settings(
    name           := "typed-ontology",
    publish / skip := true
  )
//lazy val typeSet = project
//  .in(file("type-set"))
//  .settings(
//    name := "type-set",
//  )
//  .settings(commonSettings *)

lazy val typedOntologyMetaMeta = project
  .in(file("typed-ontology-metameta"))
  .settings(
    name := "typed-ontology-metameta",
    fs2
  )
  .settings(commonSettings*)

lazy val typedOntologyMeta = project
  .in(file("typed-ontology-meta"))
  .settings(
    name := "typed-ontology-meta"
  )
  .dependsOn(typedOntologyMetaMeta)
  .settings(commonSettings: _*)

lazy val typedOntologySimpleMeta = project
  .in(file("typed-ontology-simple-meta"))
  .settings(
    name := "typed-ontology-simple-meta"
  )
  .dependsOn(typedOntologyMetaMeta)
  .settings(commonSettings*)

lazy val ontologyExample1 = project
  .in(file("ontology-example1"))
  .settings(
    name           := "ontology-example1",
    publish / skip := true
  )
  .dependsOn(
    typedOntologySimpleMeta,
    typedOntologyMetaMeta
  )
  .settings(commonSettings*)

val quillVersion = "4.8.1"
lazy val ontologyQuill = project
  .in(file("ontology-quill"))
  .settings(
    name           := "ontology-quill",
    publish / skip := true
  )
  .dependsOn(
    typedOntologySimpleMeta,
    typedOntologyMetaMeta
  )
  .settings(commonSettings*)
  .settings(
    libraryDependencies ++= Seq(
      // postgres driver
      "org.postgresql" % "postgresql" % "42.7.0",
      // Syncronous JDBC Modules
      "io.getquill" %% "quill-jdbc" % quillVersion,
      // // Or ZIO Modules
      // "io.getquill" %% "quill-jdbc-zio" % quillVersion,
      // // Or Cassandra
      // "io.getquill" %% "quill-cassandra" % quillVersion,
      // // Or Cassandra + ZIO
      // "io.getquill" %% "quill-cassandra-zio" % quillVersion,
      // // Add for Caliban Integration
      // "io.getquill" %% "quill-caliban" % quillVersion
    )
  )
