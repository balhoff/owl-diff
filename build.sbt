organization  := "org.geneontology"

name          := "owl-diff"

version       := "2.0.0"

publishMavenStyle := true

publishTo := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
  else localStaging.value
}

Test / publishArtifact := false

licenses := Seq("BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause"))

homepage := Some(url("https://github.com/balhoff/owl-diff"))

scalaVersion  := "2.13.18"

crossScalaVersions := Seq("2.13.18")

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

Test / scalacOptions ++= Seq("-Yrangepos")

testFrameworks += new TestFramework("utest.runner.Framework")

libraryDependencies ++= {
  Seq(
    "net.sourceforge.owlapi" %  "owlapi-distribution" % "5.5.1",
    "org.apache.commons"     %  "commons-text"        % "1.15.0",
    "com.lihaoyi"            %% "utest"               % "0.8.9"  % Test,
    "com.outr"               %% "scribe-slf4j2"       % "3.19.0" % Test
  )
}

pomExtra :=
  <scm>
    <url>git@github.com:balhoff/owl-diff.git</url>
    <connection>scm:git:git@github.com:balhoff/owl-diff.git</connection>
  </scm>
    <developers>
      <developer>
        <id>balhoff</id>
        <name>Jim Balhoff</name>
        <email>jim@balhoff.org</email>
      </developer>
    </developers>
