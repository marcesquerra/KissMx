import SonatypeKeys._
import sbtrelease._
import ReleaseStateTransformations._
import ReleaseKeys._
import xerial.sbt.Sonatype.SonatypeKeys

sonatypeSettings

releaseSettings

name := "KissMx"

organization := "com.bryghts.kissmx"

profileName  := "com.bryghts"

scalaVersion := "2.10.3"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.10.2"

libraryDependencies += "org.specs2" %% "specs2" % "2.2.2" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"

libraryDependencies += "junit" % "junit" % "4.11" % "test"

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

scalacOptions in Test ++= Seq("-Yrangepos")

publishMavenStyle := true

publishArtifact in Test := false

pomExtra := (
  <url>http://www.brights.com</url>
  <licenses>
    <license>
    <name>mit</name>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:marcesquerra/KissMx.git</url>
    <connection>scm:git:git@github.com:marcesquerra/KissMx.git</connection>
  </scm>
  <developers>
    <developer>
      <name>Marc Esquerrà i Bayo</name>
      <email>esquerra@bryghts.com</email>
    </developer>
  </developers>
)

releaseProcess := Seq[ReleaseStep](
	checkSnapshotDependencies,                    // : ReleaseStep
	inquireVersions,                              // : ReleaseStep
	runClean,                                     // : ReleaseStep
	runTest,                                      // : ReleaseStep
	setReleaseVersion,                            // : ReleaseStep
	commitReleaseVersion,                         // : ReleaseStep, performs the initial git checks
	tagRelease,                                   // : ReleaseStep
	releaseTask(PgpKeys.publishSigned),           // : ReleaseStep, checks whether `publishTo` is properly set up
	releaseTask(SonatypeKeys.sonatypeReleaseAll), // : ReleaseStep, checks whether `publishTo` is properly set up
	setNextVersion,                               // : ReleaseStep
	commitNextVersion,                            // : ReleaseStep
	pushChanges                                   // : ReleaseStep, also checks that an upstream branch is properly configured
)