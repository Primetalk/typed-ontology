val scala3Version = "3.6.4"
val mainVersion   = "0.2.8"

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

val catsEffect = "org.typelevel" %% "cats-effect" % "3.6.0"
val catsCore   = "org.typelevel" %% "cats-core"   % "2.13.0"
val fs2 = libraryDependencies ++= Seq(
  "co.fs2" %% "fs2-core" % "3.12.0",
  "co.fs2" %% "fs2-io"   % "3.12.0"
)

val commonSettings = Seq(
  scalaVersion := scala3Version,
  scalacOptions ++= Seq(
    // "-Xmax-inlines=50",
    "-deprecation"
  ),
  libraryDependencies ++= Seq(
    catsCore,
    //"io.github.kitlangton" %% "quotidian"       % "0.0.9",
    "com.novocode"          % "junit-interface" % "0.11"   % Test,
    "org.scalacheck"       %% "scalacheck"      % "1.18.1" % Test,
    "org.scalatest"        %% "scalatest"       % "3.2.19" % Test
  )
)

lazy val root = (project in file("."))
  .aggregate(
    typedOntologyMetaMeta,
    typedOntologyMeta,
    typedOntologySimpleMeta,
    `ontology-example2`
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

//lazy val ontologyExample1 = project
//  .in(file("ontology-example1"))
//  .settings(
//    name           := "ontology-example1",
//    publish / skip := true
//  )
//  .dependsOn(
//    typedOntologySimpleMeta,
//    typedOntologyMetaMeta
//  )
//  .settings(commonSettings*)

lazy val `ontology-example2` = project
  .in(file("ontology-example2"))
  .settings(
    name           := "ontology-example2",
    publish / skip := true
  )
  .dependsOn(
    `type-class-schema`
  )
  .settings(commonSettings*)

val quillVersion = "4.8.4"

val postgresDriver = "org.postgresql" % "postgresql" % "42.7.3"
val quillJdbc = "io.getquill" %% "quill-jdbc" % quillVersion // Synchronous JDBC Modules

lazy val ontologyQuillParser = project
  .in(file("ontology-quill-parser"))
  .settings(
    name           := "ontology-quill-parser",
    publish / skip := true
  )
  .dependsOn(
    typedOntologySimpleMeta,
    typedOntologyMetaMeta
  )
  .settings(commonSettings*)
  .settings(
    libraryDependencies ++= Seq(
      postgresDriver,
      quillJdbc,
    )
  )

lazy val ontologyQuill = project
  .in(file("ontology-quill"))
  .settings(
    name           := "ontology-quill",
    publish / skip := true
  )
  .dependsOn(
    typedOntologySimpleMeta,
    typedOntologyMetaMeta,
    ontologyQuillParser
  )
  .settings(commonSettings*)
  .settings(
    libraryDependencies ++= Seq(
      postgresDriver,
      quillJdbc,
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

lazy val `type-class-schema` = project
  .in(file("type-class-schema"))
  .settings(
    name           := "type-class-schema",
    publish / skip := true,
    scalacOptions ++= Seq(
      "experimental",
      "language:experimental.namedTuples"
    )
  )
  .dependsOn()
  .settings(commonSettings*)
