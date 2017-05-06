package concurrent

import data._
import java.util.concurrent.{ExecutorService, CountDownLatch, Callable}
import java.util.concurrent.atomic.AtomicReference
import scala.util.control.NonFatal

object NonblockingPar {

  trait Future[A] {
    def apply(f: A => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a =>
      ref.set(a)
      latch.countDown
    }
    latch.await
    ref.get
  }

  def unit[A](a: A): Par[A] = es => {
    new Future[A] {
      def apply(f: A => Unit) = f(a)
    }
  }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit {
      new Callable[Unit] {
        def call = r
      }
    }

  def fork[A](a: => Par[A]): Par[A] = es => {
    new Future[A] {
      def apply(f: A => Unit): Unit = eval(es)(a(es)(f))
    }
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = es => {
    new Future[C] {
      def apply(cb: C => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None
        val combiner = Actor[Either[A,B]](es) {
          case Left(a) => br match {
            case None => ar = Some(a)
            case Some(b) => eval(es)(cb(f(a,b)))
          }
          case Right(b) => ar match {
            case None => br = Some(b)
            case Some(a) => eval(es)(cb(f(a,b)))
          }
        }
        a(es)(a => combiner ! Left(a))
        b(es)(b => combiner ! Right(b))
      }
    }
  }

  // exercise 7.10, this does not really work
  def runSafe[A](es: ExecutorService)(p: Par[A]): Either[Throwable, A] = {
    val ref = new AtomicReference[Either[Throwable, A]]
    val latch = new CountDownLatch(1)
    try {
      p(es) { a =>
        ref.set(Right(a))
        latch.countDown
      }
    } catch {
      case NonFatal(t) =>
        ref.set(Left(t))
        latch.countDown
    }
    latch.await
    ref.get
  }

  // some common functions that are implemented in Par

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)
  def equals[A](left: Par[A], right: Par[A]): Par[Boolean] = map2(left, right)(_ == _)

  def sequence[A](pars: List[Par[A]]): Par[List[A]] = {
    pars.foldRight(unit(List.empty[A])) {
      case (next, acc) => map2(next, acc)(Cons(_,_))
    }
  }

  def map[A,B](par: Par[A])(f: A => B): Par[B] = map2(par, unit(()))((a,_) => f(a))
  def flatMap[A,B](a: Par[A])(f: A => Par[B]): Par[B] = es => f(run(es)(a))(es)
  def join[A](a: Par[Par[A]]): Par[A] = es => run(es)(a)(es)
}
