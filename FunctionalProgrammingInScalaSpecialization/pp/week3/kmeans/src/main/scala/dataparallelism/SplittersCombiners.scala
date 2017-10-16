package dataparallelism

import java.util.concurrent

import common._

import scala.concurrent.forkjoin.ForkJoinTask


abstract class SplittersCombiners[A] extends Iterator[A]{
//  val threshold = 10000
//
//  override def foldLeft[B](z: B)(f: (B, A) => B): B = {
//    var res = z
//    while (hasNext) {
//      res = f(res, next())
//    }
//    res
//  }
//
//  trait Splitter[T] extends Iterator[T] {
//    def split: Seq[Splitter[T]]
//    def remaining: Int
//
//    override def fold[T](z: T)(f: (T, T) => T): T = {
//      if (remaining < threshold) {
//        foldLeft(z)(f)
//      } else {
//        val children: Seq[concurrent.ForkJoinTask[T]] = for (child <- split) yield task { child.fold(z)(f) }
//        children.map(_.join()).foldLeft(z)(f)
//      }
//    }
//
//  }
}