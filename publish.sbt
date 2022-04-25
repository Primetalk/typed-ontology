ThisBuild / organization := "ru.primetalk"
ThisBuild / organizationName := "Primetalk"
ThisBuild / organizationHomepage := Some(url("https://primetalk.github.io/"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/Primetalk/typed-ontology"),
    "scm:https://github.com/Primetalk/typed-ontology.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id    = "Primetalk",
    name  = "Arseniy Zhizhelev",
    email = "zhizhelev@primetalk.ru",
    url   = url("https://primetalk.github.io/")
  )
)

ThisBuild / description := "Typed ontology meta projects."
ThisBuild / licenses := List("MIT" -> new URL("https://opensource.org/licenses/MIT"))
ThisBuild / homepage := Some(url("https://github.com/Primetalk/typed-ontology"))

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) 
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else 
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true

pgpSecretRing := pgpPublicRing.value

usePgpKeyHex("9A3032D0")

