enablePlugins(JavaServerAppPackaging)

organization  := "org.renci"

name          := "owl-diff"

version       := "0.1"

licenses := Seq("BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause"))

maintainer in Linux := "James Balhoff <balhoff@renci.org>"

packageSummary in Linux := "OWL diff server"

packageDescription := "A web service for OWL diffs"

scalaVersion  := "2.12.3"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

scalacOptions in Test ++= Seq("-Yrangepos")

//mainClass in Compile := Some("org.renci.ncit.Main")

fork in Test := true

libraryDependencies ++= {
  Seq(
    "net.sourceforge.owlapi" %  "owlapi-distribution" % "5.1.2",
    "org.scalaz"             %% "scalaz-core"         % "7.2.16",
    "org.apache.commons"     %  "commons-text"        % "1.1",
    "com.typesafe.akka"      %% "akka-stream"         % "2.5.4",
    "com.typesafe.akka"      %% "akka-actor"          % "2.5.4",
    "com.typesafe.akka"      %% "akka-http"           % "10.0.10"
  )
}
