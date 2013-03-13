import com.typesafe.sbt.SbtScalariform.{ScalariformKeys, scalariformSettings}
import scalariform.formatter.preferences._

name := "fibonacci-a-day"

organization := "org.entish"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.14" % "test"
)

initialCommands in console := """
  | import org.entish.FibonacciADay._
  | import java.io.File
  """.stripMargin

seq(scalariformSettings:_*)

ScalariformKeys.preferences :=
  (FormattingPreferences()
        setPreference(IndentSpaces, 2)
        setPreference(AlignParameters, true)
        setPreference(AlignSingleLineCaseStatements, true)
        setPreference(DoubleIndentClassDeclaration, true)
        setPreference(RewriteArrowSymbols, true)
        setPreference(PreserveSpaceBeforeArguments, true)
        setPreference(IndentWithTabs, false))


