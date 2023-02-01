package com.rockthejvm.part2effects.mysolutions

import cats.effect.IO

import scala.io.StdIn

object IOIntro {

  val firstIO: IO[Int] = IO.pure(42) // should not have side effects, evaluates eagerly.
  val delayedIO: IO[Int] = IO.delay {       // evaluates lazy
    println("producing integer")
    54
  }

  val delayedIO_v2: IO[Int] = IO {          // apply == delay
    println("I'm producing an integer")
    54
  }

  //map, flatmap
  val improved = firstIO.map(_ * 2)
  val printed = firstIO.flatMap(vval => IO.delay(println(vval)))

  //mapN - combine IO effects as tuples
  import cats.syntax.apply._
  val combinedMeaningOfLife: IO[Int] = (firstIO, improved).mapN(_ + _)
  def smallProgram_2: IO[Unit] = (IO(StdIn.readLine()), IO(StdIn.readLine())).mapN(_ + _).map(println)

  def smallProgram: IO[Unit] = for {
    l1 <- IO(StdIn.readLine())
    l2 <- IO(StdIn.readLine())
    _ <- IO.delay(println(s"$l1 and $l2"))
  } yield ()

/**
* Exercises
* */

// 1 - sequence two IOs and take the result of the LAST one
  def sequnceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] = {
    for {
      a <- ioa
      b <- iob
    } yield b
  }

  def sequnceTakeLast_v2[A, B](ioa: IO[A], iob: IO[B]): IO[B] = {
    ioa.flatMap(_ => iob)
  }

  def sequnceTakeLast_v3[A, B](ioa: IO[A], iob: IO[B]): IO[B] = {
      ioa *> iob //andThen
  }

  def sequnceTakeLast_v4[A, B](ioa: IO[A], iob: IO[B]): IO[B] = {
    ioa >> iob //endThen called by-name call
  }

  // 2 take first but execute both
  def sequnceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] = {
    for {
      b <- iob
      a <- ioa
    } yield a
  }

  def sequnceTakeFirst_v2[A, B](ioa: IO[A], iob: IO[B]): IO[A] = {
   ioa <* iob
  }

  //3  run IO forever
  def forever[A](io: IO[A]): IO[A] =  io.flatMap(iio => forever(io))
  def forever_v2[A](io: IO[A]): IO[A] =  io >> forever_v2(io) //will evaluated earlier so ok, its tail recursive
  def forever_v3[A](io: IO[A]): IO[A] =  io *> forever_v3(io) //eagerly evaluation *> crashes it with stack overflow
  def forever_v4[A](io: IO[A]): IO[A] =  io.foreverM

  // 4 convert IO to a different type
  def convert[A, B](ioa: IO[A], value: B): IO[B] = ioa.map(_ => value)
  def convert_v2[A, B](ioa: IO[A], value: B): IO[B] = ioa.as(value)

  //5 discard value inside IO just return IO
  def asUnit[A](ioa: IO[A]): IO[Unit] = ioa.map(_ => ())
  def asUnit_v2[A](ioa: IO[A]): IO[Unit] = ioa.as(()) // bad practice
  def asUnit_v3[A](ioa: IO[A]): IO[Unit] = ioa.void // bad practice

  //6 fix stack recursion
  def sum(n: Int): Int = if (n <= 0) 0 else n + sum(n - 1)

  def sumIO(n: Int): IO[Int] =
    if (n <= 0) IO(0)
    else for {
      cur <- IO(n)
      prev <- sumIO(n - 1)
    } yield cur + prev

  //7 write fibonacci IO that does not crash on recursion
  // hints: use recursion, ignore exponential complexity, use flatMap heavily

  def fibonacci(n: Int): IO[BigInt] = {
    if (n == 0) IO(0)
    else if (n == 1) IO(1)
    else {
      fibonacci(n - 2).flatMap(p => fibonacci(n - 1).map(pp => p + pp)) //it works
//      for {
//        last <- IO.defer(fibonacci(n - 2))
//        prev <- IO.defer(fibonacci(n - 1))
//      } yield last + prev
    }
  }

  def nonCatsFibo(n: Int): Int = {
    if (n == 0) 0
    else if (n == 1) 1
    else {nonCatsFibo(n - 1) + nonCatsFibo(n - 2)}
  }

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global // runtime platform for using IO
//    println(delayedIO.unsafeRunSync())
//    println()
//    smallProgram.unsafeRunSync()
//    smallProgram_2.unsafeRunSync()
    sequnceTakeLast(IO(5), IO("25")).map(println).unsafeRunSync()
    println(sumIO(300000).unsafeRunSync())
//    println(sum(300000))
//    println(nonCatsFibo(20000))
    (1 to 100) foreach ( i => println(fibonacci(i).unsafeRunSync()))
  }

}
