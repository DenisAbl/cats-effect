package com.rockthejvm

import cats.effect.IO

package object utils {
  implicit class ExThreads[A](val io: IO[A]) {
    def debug: IO[A] = for {
      a <- io
      t = Thread.currentThread().getName
      _ = println(s"[$t] $a")
    } yield a
  }
}
