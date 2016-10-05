scalaVersion := "2.11.8"


// excludeFilter in unmanagedSources ~= { _ || ".#*.scala" }
// scalaSource in Compile := baseDirectory.value

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.4"
libraryDependencies += "org.scalacheck" % "scalacheck_2.11" % "1.13.2" % "test"

libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "2.14.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
libraryDependencies += "org.typelevel" %% "cats" % "0.7.2"

libraryDependencies += "com.espertech" % "esper" % "4.11.0"
