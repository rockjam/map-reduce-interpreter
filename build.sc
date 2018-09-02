import mill._, scalalib._

object root extends ScalaModule {
  def scalaVersion = "2.12.6"

  def ivyDeps = Agg(
    ivy"com.lihaoyi::fastparse:1.0.0"
  )

  def mainClass = Some("com.github.rockjam.mapreduce.Main")

  object test extends Tests {
    def testFrameworks = Seq("utest.runner.Framework")
    def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.6.3")
  }
}
