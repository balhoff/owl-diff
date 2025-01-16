organization  := "org.geneontology"

name          := "owl-diff"

version       := "1.3.0"

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

licenses := Seq("BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause"))

homepage := Some(url("https://github.com/balhoff/owl-diff"))

scalaVersion  := "2.13.16"

crossScalaVersions := Seq("2.13.16")

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

Test / scalacOptions ++= Seq("-Yrangepos")

testFrameworks += new TestFramework("utest.runner.Framework")

libraryDependencies ++= {
  Seq(
    "net.sourceforge.owlapi" %  "owlapi-distribution" % "4.5.29",
    "org.apache.commons"     %  "commons-text"        % "1.12.0",
    "com.lihaoyi"            %% "utest"               % "0.8.3"  % Test,
    "com.outr"               %% "scribe-slf4j2"       % "3.15.2" % Test
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
