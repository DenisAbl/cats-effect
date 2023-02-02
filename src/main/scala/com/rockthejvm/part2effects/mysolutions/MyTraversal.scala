package com.rockthejvm.part2effects.mysolutions

import cats.effect.{IO, IOApp}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random

object MyTraversal extends  IOApp.Simple {

  implicit val ex = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val workLoad: List[String] = List("1 2 3 4", "1 2 3", "1 2 3 4 5 6 7")

  def heavyComputations(string: String): Future[Int] = Future {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }
  def clunkyFutures = {
    val futures: List[Future[Int]] = workLoad.map(heavyComputations)
    futures.foreach(_.foreach(println))
  }
  //Future[List[Int]] would be hard to obtain, but we have std library method traverse


  // traverse
  import cats.Traverse
  val listTraverse: Traverse[List] = Traverse[List]

  def traverseFutures = {
    val traversed: Future[List[Int]] = listTraverse.traverse(workLoad)(heavyComputations)
    traversed.foreach(println)
  }

  // traverse IO
  import com.rockthejvm.utils._
  def computeAsIO(string: String): IO[Int] = {
    IO {
      Thread.sleep(Random.nextInt(1000))
      string.split(" ").length
    }.debug
  }

  val ios: List[IO[Int]] = workLoad.map(computeAsIO)
  val singleIO: IO[List[Int]] = listTraverse.traverse(workLoad)(computeAsIO)


//  parallel traverse
  import cats.syntax.parallel._
  val paralellSingleIO: IO[List[Int]] = workLoad.parTraverse(computeAsIO)

  /**
   * Exercises
  * */
  def sequence[A](list: List[IO[A]]): IO[List[A]] = {
    listTraverse.traverse(list)(identity)
  }

  def sequence_v2[F[_] : Traverse, A](containerOfIOs: F[IO[A]]): IO[F[A]] = {
    val contTraverse = Traverse[F]
    contTraverse.traverse(containerOfIOs)(identity)
  }

  def parSequence[A](list: List[IO[A]]): IO[List[A]] = {
    list.parTraverse(identity)
  }
  def parSequence_v2[F[_] : Traverse, A](containerOfIOs: F[IO[A]]): IO[F[A]] = {
    val contTraverse = Traverse[F]
    containerOfIOs.parTraverse(identity)
//    containerOfIOs.parSequence //existing API
  }

  // existing sequence API
  val singleIO_v2: IO[List[Int]] = listTraverse.sequence(ios)

  // parallel sequencing
  val parallelSingleIO_v2: IO[List[Int]] = parSequence(ios) // from the exercise
  val parallelSingleIO_v3: IO[List[Int]] = ios.parSequence // extension method from the Parallel syntax package

  override def run: IO[Unit] = {
    import cats.effect.unsafe.implicits.global
    singleIO.void
  }

}
