val attoVersion       = "0.9.5"
val catsVersion       = "2.9.0"
val catsEffectVersion = "3.5.0"
val catsParseVersion  = "0.3.7"
val circeVersion      = "0.14.5"
val http4sVersion     = "0.23.19"
val jsoupVersion      = "1.16.1"
val munitVersion      = "1.0.0-M6"
val sttpClientVersion = "4.0.0-M1"
val sttpOpenaiVersion = "0.0.6"
val unindentVersion   = "1.8.0"

lazy val chat = project.in(file(".")).settings(commonSettings("chat"))

def commonSettings(projectName: String) =
  Seq(
    name         := projectName,
    version      := "0.0.1-SNAPSHOT",
    scalaVersion := "3.2.2",
    scalacOptions ++= commonScalacOptions,
    libraryDependencies ++= commonDependencies,
  )

lazy val commonScalacOptions = Seq(
  "-encoding",
  "UTF-8",                 // source files are in UTF-8
  "-deprecation",          // warn about use of deprecated APIs
  "-unchecked",            // warn about unchecked type parameters
  "-feature",              // warn about misused language features
  "-language:higherKinds", // allow higher kinded types without `import scala.language.higherKinds`
  "-Xfatal-warnings",      // turn compiler warnings into errors
  "-indent",
  "-rewrite",
)

lazy val commonDependencies = Seq(
  "com.davegurnell"               %% "unindent"            % unindentVersion,
  "io.circe"                      %% "circe-core"          % circeVersion,
  "io.circe"                      %% "circe-generic"       % circeVersion,
  "io.circe"                      %% "circe-parser"        % circeVersion,
  "org.http4s"                    %% "http4s-dsl"          % http4sVersion,
  "org.http4s"                    %% "http4s-ember-server" % http4sVersion,
  "org.http4s"                    %% "http4s-ember-client" % http4sVersion,
  "org.jsoup"                      % "jsoup"               % jsoupVersion,
  "org.scalameta"                 %% "munit"               % munitVersion % Test,
  "org.scalameta"                 %% "munit-scalacheck"    % munitVersion % Test,
  "org.tpolecat"                  %% "atto-core"           % attoVersion,
  "org.typelevel"                 %% "cats-core"           % catsVersion,
  "org.typelevel"                 %% "cats-effect"         % catsEffectVersion,
  "org.typelevel"                 %% "cats-parse"          % catsParseVersion,
  "com.softwaremill.sttp.client4" %% "core"                % sttpClientVersion,
  "com.softwaremill.sttp.client4" %% "cats"                % sttpClientVersion,
  "com.softwaremill.sttp.openai"  %% "core"                % sttpOpenaiVersion,
)
