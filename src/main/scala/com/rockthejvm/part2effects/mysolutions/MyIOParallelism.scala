package com.rockthejvm.part2effects.mysolutions

import cats.effect.{IO, IOApp}

import scala.StringContext.InvalidEscapeException

object MyIOParallelism extends IOApp.Simple {


  val anisIO = IO(s"[${Thread.currentThread.getName}] Ani")
  val kamranIO = IO(s"[${Thread.currentThread.getName}] Kamran")

  val composedIO = for {
    ani <- anisIO
    kamran <- kamranIO
  } yield s"$ani and $kamran"


  import com.rockthejvm.utils.ExThreads
  val fisrtTry: IO[Int] = IO(42)
  val secondTry: IO[String] = IO("Scala")
  import cats.syntax.apply._
  val combined = (fisrtTry.debug, secondTry.debug).mapN((num, string) => s"we have composed $num adn $string")


  //Parallelism

  //convert sequential IO to parallel
  import cats.Parallel

  val parIO1: IO.Par[Int] = Parallel[IO].parallel(fisrtTry.debug)
  val parIO2: IO.Par[String] = Parallel[IO].parallel(secondTry.debug)
  import cats.effect.implicits._
  val parIO3: IO.Par[String] = (parIO1, parIO2).mapN((f, s) => s"composed first: $f and second: $s")
  //turn back to sequential
  val seq1 = Parallel[IO].sequential(parIO3)

  //shorthand
  import cats.syntax.parallel._
  //parMapN
  val combined3 = (fisrtTry.debug, secondTry.debug).parMapN((num, string) => s"we have composed $num adn $string")

  //compose success and failure
  val failure: IO[String] = IO.raiseError(new RuntimeException("Failed because I want so"))
  val parWithFailure: IO[String] = (fisrtTry.debug, failure.debug).parMapN(_ + _)

  //compose failure and failure
  val anotherFailure: IO[String] = IO.raiseError(new IllegalArgumentException("Second Failure"))
  val twoFailures: IO[String] = (IO(Thread.sleep(1000)) >> failure.debug, anotherFailure.debug).parMapN(_ + _)

  override def run: IO[Unit] = twoFailures.debug.void
}
