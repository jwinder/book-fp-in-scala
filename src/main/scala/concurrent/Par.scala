package concurrent

import data._
import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(par: Par[A]): Future[A] = par(es)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  // exercise 7.1
  def map2_naive[A,B,C](left: Par[A], right: Par[B])(f: (A,B) => C): Par[C] = es => {
    val l = left(es)
    val r = right(es)
    UnitFuture(f(l.get, r.get))
  }

  def fork[A](a: => Par[A]): Par[A] = es => {
    es.submit {
      new Callable[A] {
        def call = a(es).get
      }
    }
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // exercise 7.3
  def map2[A,B,C](left: Par[A], right: Par[B])(f: (A,B) => C): Par[C] = es => {
    new Future[C] {
      private val leftF = left(es)
      private val rightF = right(es)

      private val result = new atomic.AtomicReference[C]()
      private def resultIsSet = result.get() != null
      private def resultIsNotSet = result.get() == null

      private def getValue(timeout: Long, units: TimeUnit): C = {
        if (resultIsSet) {
          result.get()
        } else {
          val nanosLeft = TimeUnit.NANOSECONDS.convert(timeout, units)
          def getTimed[A](f: Future[A], timeoutNanos: Long): (A, Long) = {
            val start = System.nanoTime
            val a = f.get(timeoutNanos, TimeUnit.NANOSECONDS)
            val end = System.nanoTime
            val nanos = end - start
            (a, nanos)
          }
          val (a, aNanos) = getTimed(leftF, nanosLeft)
          val (b, bNanos) = getTimed(rightF, nanosLeft - aNanos)
          val c = f(a,b)
          result.set(c)
          c
        }
      }

      def get: C = getValue(Long.MaxValue, TimeUnit.NANOSECONDS)
      def get(timeout: Long, units: TimeUnit): C = getValue(timeout, units)
      def cancel(evenIfRunning: Boolean): Boolean = leftF.cancel(evenIfRunning) || rightF.cancel(evenIfRunning)
      def isDone: Boolean = resultIsSet
      def isCancelled: Boolean = leftF.isCancelled || rightF.isCancelled
    }
  }

  // exercise 7.4
  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A,B](par: Par[A])(f: A => B): Par[B] = map2(par, unit(()))((a,_) => f(a))

  def sortPar(pars: Par[List[Int]]): Par[List[Int]] = map(pars)(_.sorted)

  // exercise 7.5
  def sequence[A](pars: List[Par[A]]): Par[List[A]] = {
    pars.foldRight(unit(List.empty[A])) {
      case (next, acc) => map2(next, acc)(Cons(_,_))
    }
  }

  def parMap[A,B](as: List[A])(f: A => B): Par[List[B]] = Par.fork {
    sequence(as.map(asyncF(f)))
  }

  // exercise 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = Par.fork {
    sequence(as.filter(f).map(lazyUnit(_)))
  }

  // not sure if there is a better way to do this without having the .get()
  def map3[A,B,C,D](a: Par[A], b: Par[B], c: Par[C])(f: (A,B,C) => D): Par[D] = es => {
    val fa: B => C => D = map(a)(f.curried)(es).get()
    map2(b,c)(Function.uncurried(fa))(es)
  }
  def map4[A,B,C,D,E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A,B,C,D) => E): Par[E] = es => {
    val fa: B => C => D => E = map(a)(f.curried)(es).get()
    map3(b,c,d)(Function.uncurried(fa))(es)
  }
  def map5[A,B,C,D,E,F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A,B,C,D,E) => F): Par[F] = es => {
    val fa: B => C => D => E => F = map(a)(f.curried)(es).get()
    map4(b,c,d,e)(Function.uncurried(fa))(es)
  }

  def eachWordCount(paragraphs: List[String]): Par[List[Int]] = {
    parMap(paragraphs)(_.split("\\s").size)
  }

  def equals[A](left: Par[A], right: Par[A]): Par[Boolean] = map2(left, right)(_ == _)

  // exercise 7.8, fork can deadlock for fixed thread pools
  // exercise 7.9
  def deadlockFork(numThreads: Int): Unit = {
    val es = Executors.newFixedThreadPool(numThreads)
    List.fill(numThreads)(1).foldLeft(lazyUnit(1)) {
      case (par, n) => fork(par)
    }(es).get()
  }

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  def choiceBool[A](cond: Par[Boolean])(truePar: Par[A], falsePar: Par[A]): Par[A] = es => {
    if (cond(es).get) {
      truePar(es)
    } else {
      falsePar(es)
    }
  }

  // exercise 7.11
  def choiceNList[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
    choices(n(es).get)(es)
  }

  def choiceBool2[A](cond: Par[Boolean])(truePar: Par[A], falsePar: Par[A]): Par[A] = es => {
    val n = Par.map(cond)(if (_) 0 else 1)
    choiceNList(n)(List(truePar, falsePar))(es)
  }

  // exercise 7.12
  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] = es => {
    choices(key(es).get)(es)
  }

  // exercise 7.13
  def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = es => {
    choices(pa(es).get)(es)
  }

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
    chooser[Int,A](n) { n => choices(n) }(es)
  }

  // exercise 7.14
  def flatMap[A,B](a: Par[A])(f: A => Par[B]): Par[B] = es => f(a(es).get)(es)
  def join[A](a: Par[Par[A]]): Par[A] = es => (a(es).get())(es)
  def flatMapViaJoin[A,B](a: Par[A])(f: A => Par[B]): Par[B] = join(map(a)(f))
  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] = flatMap(a)(identity)
}
