package com.rockthejvm.part2effects.mysolutions

import cats.effect.{ExitCode, IO, IOApp}

import scala.io.StdIn

object MyIOApps {
  val program = for {
    line <- IO(StdIn.readLine())
    _ <- IO(println(s"You have entered: $line"))
  } yield ()
}

object TestApp {
  import MyIOApps._

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
    program.unsafeRunSync()
  }
}

object FirstIOApp extends IOApp {
  import MyIOApps._
  override def run(args: List[String]): IO[ExitCode] = program.as(ExitCode.Success)
}

object SimpleIOApp extends IOApp.Simple {
  import MyIOApps._
  override def run = program
}