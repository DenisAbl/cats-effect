package com.rockthejvm.part2effects.mysolutions

import cats.effect.IO

import java.io.IOException
import scala.util.{Failure, Success, Try}

object IOErrorHandling {

  //IO: pure, delay, defer - it was done previously
  //now create failed effects

  val failedComp: IO[Int] = IO.delay(throw new RuntimeException("FAILED"))
  val failure: IO[Int] = IO.raiseError(new RuntimeException("FAILED")) // more readable
  val failure2: IO[Int] = IO.raiseError(new IOException("FAILED")) // more readable

  //handle exceptions
  val fallbackEx = failure.handleErrorWith {
    case _: IllegalArgumentException => IO(println("NOT OUR EXCEPTION"))
    case _: RuntimeException => IO(println("OUR EXCEPTION"))
      //could not be partial function - will be failed without common case
  }

  //Turn into the Either
  val effectAsEither: IO[Either[Throwable, Int]] = failure.attempt

  //redeem: transform the failure and the success in one go
  val resultAsString: IO[String] = failure.redeem(ex => s"FAIL: $ex", value => s"SUCCESS: got $value")
  val resultAsEffect: IO[Unit] = failure.redeemWith(ex => IO(println(s"FAIL: $ex")), value => IO(println(s"SUCCESS: got $value")))

  // 1 - construct potentially failed IOs from standard data types (Option, Try, Either)

//  API has methods:
//  IO.fromTry()
//  IO.fromEither()
//  IO.fromOption()
//  IO.fromFuture()
  def option2IO[A](option: Option[A])(ifEmpty: Throwable): IO[A] = {
    option match {
      case Some(value) => IO(value)
      case None => IO.raiseError(ifEmpty)
    }
  }
  def try2IO[A](aTry: Try[A]): IO[A] = {
    aTry match {
      case Failure(exception) => IO.raiseError(exception)
      case Success(value) => IO.delay(value)
    }
  }
  def either2IO[A](anEither: Either[Throwable, A]): IO[A] = anEither match {
    case Left(ex) => IO.raiseError(ex)
    case Right(value) => IO.delay(value)
  }

  // 2 - handleError, handleErrorWith
  def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A] = io.handleError(handler)
  def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] = io.handleErrorWith(handler)

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.IORuntime.global //just to try, can import directly instance from implicit package
    implicit val glo = global
//    failedComp.unsafeRunSync()
//    failure.unsafeRunSync()
//    fallbackEx.unsafeRunSync()
//    println(resultAsString.unsafeRunSync())
    resultAsEffect.unsafeRunSync()
  }

}
