scalaVersion := "2.12.15"


scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xlint",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard"
  // 警告をエラーにする（お好みに応じて）
  //, "-Xfatal-warnings"
)