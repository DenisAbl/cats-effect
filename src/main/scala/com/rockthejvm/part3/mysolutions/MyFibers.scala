package com.rockthejvm.part3.mysolutions

import cats.effect.IO.{IOCont, Uncancelable}
import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, FiberIO, IO, IOApp}

import scala.concurrent.duration.{DurationInt, FiniteDuration}

object MyFibers extends IOApp.Simple {

  val intIO = IO.pure(55)
  val stringIO = IO.pure("Scala")

  import  com.rockthejvm.utils._
  def sameThreadIOs() = for {
    _ <- intIO.debug
    _ <- stringIO.debug
  } yield ()

  //introduce the Fiber
  def createFiber: Fiber[IO, Throwable, String] = ??? // almost impossible to create manually

  // the fiber is not actually started, but the fiber allocation is wrapped in another effect
  val fiber: IO[Fiber[IO, Throwable, Int]] = intIO.debug.start

  def differentThreadIOs() = for {
    _ <- fiber
    _ <- stringIO.debug
  } yield ()

  //joining a fiber

  def runOnSomeOtherThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib <- io.start
    result <- fib.join  // an effect which waits for the fiber to terminate
  } yield result

  /*
  possible outcomes:
  - success with an IO
  - failure with an exception
  - cancelled
 */

  val someIOOnAnotherThread = runOnSomeOtherThread(intIO)
  val someResultFromAnotherThread = someIOOnAnotherThread.flatMap {
    case Succeeded(effect) => effect
    case Errored(exception) => IO(0)
    case Canceled() => IO(0)
  }

  def throwOnAnotherThread() = for {
    fib <- IO.raiseError[Int](new RuntimeException("no number for you")).start
    result <- fib.join
  } yield  result

  def testCancel() = {
    val task = IO("starting").debug >> IO.sleep(1.second) >> IO("done").debug
    val taskWithCancellationHandler = task.onCancel(IO("Fuck I am being cancelled").debug.void)

    for {
      fib <- taskWithCancellationHandler.start //on separate thread
      _ <- IO.sleep(500.millis) >> IO("canceling").debug
      _ <- fib.cancel
      result <- fib.join
    } yield  result
  }



  /**
   * Exercises:
   *  1. Write a function that runs an IO on another thread, and, depending on the result of the fiber
   *    - return the result in an IO
   *    - if errored or cancelled, return a failed IO
   *
   *  2. Write a function that takes two IOs, runs them on different fibers and returns an IO with a tuple containing both results.
   *    - if both IOs complete successfully, tuple their results
   *    - if the first IO returns an error, raise that error (ignoring the second IO's result/error)
   *    - if the first IO doesn't error but second IO returns an error, raise that error
   *    - if one (or both) canceled, raise a RuntimeException
   *
   *  3. Write a function that adds a timeout to an IO:
   *    - IO runs on a fiber
   *    - if the timeout duration passes, then the fiber is canceled
   *    - the method returns an IO[A] which contains
   *      - the original value if the computation is successful before the timeout signal
   *      - the exception if the computation is failed before the timeout signal
   *      - a RuntimeException if it times out (i.e. cancelled by the timeout)
   */
  // 1
  def processResultsFromFiber[A](io: IO[A]): IO[A] = {
    val comp: IO[Outcome[IO, Throwable, A]] = io.start.flatMap(_.join)

    comp flatMap  {
      case Succeeded(ioEffect) => ioEffect
      case Errored(exception) => IO.raiseError(exception)
      case Canceled() => IO.raiseError(new RuntimeException("IO was canceled bro"))
    }
  }

  def testTask1 = {
    IO("Starting").debug >> IO(Thread.sleep(2000)) >> IO("done").debug
  }

  def testTask1_2 = {
    IO("Starting").debug >> IO(Thread.sleep(2000)) >> IO("Canceled").start.debug.flatMap(_.cancel)
  }

  def testTask1_3 = {
    IO("Starting").debug >> IO(Thread.sleep(2000)) >> IO.raiseError(new Exception("Got some errrrrrrrs"))
  }

  // 2
  def tupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = {
    // I made sequenced execution - it's bad according to the conditions
//    ioa.start
//      .flatMap(_.join)
//      .flatMap {
//        case Canceled() => IO.raiseError(new RuntimeException("IO was canceled bro"))
//        case Errored(exception) => IO.raiseError(exception)
//        case Succeeded(fa) => iob.start.flatMap(_.join).flatMap {
//          case Canceled() => IO.raiseError(new RuntimeException("IO was canceled bro"))
//          case Errored(exception) => IO.raiseError(exception)
//          case Succeeded(fb) => for {
//            a <- fa
//            b <- fb
//           } yield (a, b)
//          }
//      }

    //Daniels solution
    val result = for {
      fiba <- ioa.start
      fibb <- iob.start
      resulta <- fiba.join
      resultb <- fibb.join
    } yield (resulta, resultb)

    result.flatMap {
      case (Succeeded(fa), Succeeded(fb)) => for {
        a <- fa
        b <- fb
      } yield (a, b)
      case (Errored(e), _) => IO.raiseError(e)
      case (_, Errored(e)) => IO.raiseError(e)
      case _ => IO.raiseError(new RuntimeException("Some computation canceled."))
    }

 }

  def testTask2 () = {
    val firstIO = IO.sleep(2.seconds) >> IO(1).debug
    val secondIO = IO.sleep(3.seconds) >> IO(2).debug
    tupleIOs(firstIO, secondIO).debug.void
  }

  // 3
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    val result = for {
      f <- io.start
      timer <- (IO.sleep(duration) >> f.cancel).start
      value <- f.join
    } yield value

    result.flatMap{
      case Canceled() => IO.raiseError(new RuntimeException("Canceled, go home"))
      case Errored(exception) => IO.raiseError(exception)
      case Succeeded(fb) => fb
    }
  }

  def testTask3() = {
    val sample1 = IO("Starting").debug >> IO(Thread.sleep(500)) >> IO("Done").debug
    val sample2 = IO("Starting").debug >> IO(Thread.sleep(2000)) >> IO("Done").debug
    val sample3 = IO("Starting").debug >> IO(Thread.sleep(999)) >> IO.raiseError(new Exception("Got some errrrrrrrs"))

    timeout(sample1, 1.seconds).debug.void
    timeout(sample2, 1.seconds).debug.void
    timeout(sample3, 1.seconds).debug.void
  }



  override def run: IO[Unit] = {
    import cats.effect.unsafe.implicits.global

    //    testCancel().debug.void
    //    runOnSomeOtherThread(intIO) //Succeeded(IO(55))
    //      .debug.void

    //1 task check:
//    processResultsFromFiber(testTask1).debug.unsafeRunSync()
//    processResultsFromFiber(testTask1_2).debug.unsafeRunSync()
//    processResultsFromFiber(testTask1_3).debug.unsafeRunSync()


//    testTask2()
    testTask3()
  }
}