package com.rockthejvm.part3.mysolutions

import cats.Traverse
import cats.effect.IO.{IOCont, Uncancelable}
import cats.effect.{Fiber, IO, IOApp, Outcome}

import scala.concurrent.TimeoutException
import scala.concurrent.duration.{DurationInt, FiniteDuration}

object MyRacingIO extends IOApp.Simple {


  import com.rockthejvm.utils._

  def runWithSleep[A](value: A, duration: FiniteDuration): IO[A] =
    (
      IO(s"starting computation: $value").debug >>
        IO.sleep(duration) >>
        IO(s"computation for $value: done") >>
        IO(value)
      ).onCancel(IO(s"computation CANCELED for $value").debug.void)

  def testRace() = {
    val meaningOfLife = runWithSleep(42, 1.second)
    val favLang = runWithSleep("Scala", 2.seconds)
    val first: IO[Either[Int, String]] = IO.race(meaningOfLife, favLang)
    /*
      - both IOs run on separate fibers
      - the first one to finish will complete the result
      - the loser will be canceled
     */

    first.flatMap {
      case Left(mol) => IO(s"Meaning of life won: $mol")
      case Right(lang) => IO(s"Fav language won: $lang")
    }
  }

  def testRacePair() = {
    val meaningOfLife = runWithSleep(42, 1.second)
    val favLang = runWithSleep("Scala", 2.seconds)
    val raceResult: IO[Either[
      (Outcome[IO, Throwable, Int], Fiber[IO, Throwable, String]), // (winner result, loser fiber)
      (Fiber[IO, Throwable, Int], Outcome[IO, Throwable, String])  // (loser fiber, winner result)
    ]] = IO.racePair(meaningOfLife, favLang)

    raceResult.flatMap {
      case Left((outMol, fibLang)) => fibLang.cancel >> IO("MOL won").debug >> IO(outMol).debug
      case Right((fibMol, outLang)) => fibMol.cancel >> IO("Language won").debug >> IO(outLang).debug
    }
  }

  /**
   * Exercises:
   * 1 - implement a timeout pattern with race
   * 2 - a method to return a LOSING effect from a race (hint: use racePair)
   * 3 - implement race in terms of racePair
   */
  // 1
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    IO.race(io, IO.sleep(duration)) flatMap {
      case Left(value) => IO(value)
      case Right(timeout) => IO.raiseError(new TimeoutException("Timeout exceeded"))
    }
  }

  // 2
  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Left((aRes, bFib)) => bFib.join.flatMap {
        case Outcome.Succeeded(fb) => fb.map(b => Right(b))
        case Outcome.Errored(e) => IO.raiseError(e)
        case Outcome.Canceled() => IO.raiseError(new Exception ("Last comp was canceled"))
      }
      case Right((aFib, bRes)) => aFib.join.flatMap {
        case Outcome.Succeeded(fa) => fa.map(a => Left(a))
        case Outcome.Errored(e) => IO.raiseError(e)
        case Outcome.Canceled() => IO.raiseError(new Exception ("Last comp was canceled"))
      }
    }


  // 3
  def simpleRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] = {
    IO.racePair(ioa, iob).flatMap {
      case Left((aRes, fiberB)) => aRes match {
        case Outcome.Succeeded(fa) => fa.map(a => Left(a))
        case Outcome.Errored(e) => IO.raiseError(e)
        case Outcome.Canceled() => fiberB.join.flatMap{
          case Outcome.Succeeded(fb) => fb.map(b => Right(b))
          case Outcome.Errored(e) => IO.raiseError(e)
          case Outcome.Canceled() => IO.raiseError(new Exception ("ioA and ioB was canceled"))
        }
      }
      case Right((fibA, bRes)) => bRes match {
        case Outcome.Succeeded(fb) => fb.map(b => Right(b))
        case Outcome.Errored(e) => IO.raiseError(e)
        case Outcome.Canceled() => fibA.join.flatMap {
          case Outcome.Succeeded(fa) => fa.map(a => Left(a))
          case Outcome.Errored(e) => IO.raiseError(e)
          case Outcome.Canceled() => IO.raiseError(new Exception("ioA and ioB was canceled"))
        }
      }
    }
  }
  override def run: IO[Unit] = {
    val ioa = IO("AAAAAAAAAAA").debug >> IO.sleep(5.seconds) >> IO("Finished AAA").debug
    val iob = IO("BBBBBBBBBBB").debug >> IO.sleep(7.seconds) >> IO.canceled
    IO.race(ioa, iob) >> IO.unit
  }
}