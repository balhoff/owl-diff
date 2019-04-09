organization  := "org.geneontology"

name          := "owl-diff"

version       := "0.1"

licenses := Seq("BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause"))

scalaVersion  := "2.12.8"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

scalacOptions in Test ++= Seq("-Yrangepos")

fork in Test := true

libraryDependencies ++= {
  Seq(
    "net.sourceforge.owlapi" %  "owlapi-distribution" % "4.5.10",
    "org.apache.commons"     %  "commons-text"        % "1.1"
  )
}
