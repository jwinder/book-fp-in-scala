package concurrent

import data._
import org.specs2.mutable.Specification
import org.specs2.specification.{Scope, After}
import java.util.concurrent._

class ParSpec extends Specification {

  "unit" in new context {
    Par.unit(5)(es).get must_== 5
    Par.unit(blockThread(5))(es).get must_== 5
  }

  "fork" in new context {
    Par.fork(Par.unit(5))(es).get must_== 5
    Par.fork(Par.unit(blockThread(5)))(es).get(10, ms) must throwA[TimeoutException]
  }

  "lazyUnit" in new context {
    Par.lazyUnit(5)(es).get must_== 5
    Par.lazyUnit(blockThread(5))(es).get must_== 5
    Par.lazyUnit(blockThread(5))(es).get(10, ms) must throwA[TimeoutException]
  }

  "map2" in new context {
    Par.map2(Par.unit(5), Par.unit(2))(_*_)(es).get must_== 10

    Par.map2(Par.lazyUnit(blockThread(5)), Par.unit(2))(_*_)(es).get must_== 10
    Par.map2(Par.lazyUnit(blockThread(5)), Par.unit(2))(_*_)(es).get(10, ms) must throwA[TimeoutException]

    Par.map2(Par.unit(5), Par.lazyUnit(blockThread(2)))(_*_)(es).get must_== 10
    Par.map2(Par.unit(5), Par.lazyUnit(blockThread(2)))(_*_)(es).get(10, ms) must throwA[TimeoutException]
  }

  "asyncF" in new context {
    Par.asyncF[Int,Int](_*2)(2)(es).get must_== 4
    Par.asyncF[Int,Int](_*2)(blockThread(2))(es).get must_== 4
  }

  "map" in new context {
    Par.map(Par.unit(List(1,2,3,4,5)))(_.size)(es).get must_== 5
  }

  "sortPar" in new context {
    Par.sortPar(Par.unit(List(5,4,3,2,1)))(es).get must_== List(1,2,3,4,5)
  }

  "sequence" in new context {
    Par.sequence(List(Par.unit(1), Par.unit(2), Par.unit(3)))(es).get must_== List(1,2,3)
  }

  "parMap" in new context {
    Par.parMap(List(1,2,3))(_*2)(es).get must_== List(2,4,6)
  }

  "parFilter" in new context {
    Par.parFilter(List(1,2,3))(_ % 2 == 0)(es).get must_== List(2)
  }

  "map3" in new context {
    Par.map3(Par.unit(2), Par.unit(2), Par.unit(2))(_*_*_)(es).get must_== 8
  }

  "map4" in new context {
    Par.map4(Par.unit(2), Par.unit(2), Par.unit(2), Par.unit(2))(_*_*_*_)(es).get must_== 16
  }

  "map5" in new context {
    Par.map5(Par.unit(2), Par.unit(2), Par.unit(2), Par.unit(2), Par.unit(2))(_*_*_*_*_)(es).get must_== 32
  }

  "eachWordCount" in new context {
    Par.eachWordCount(List("one", "two two", "three three three"))(es).get must_== List(1,2,3)
  }

  "equals" in new context {
    Par.equals(Par.unit(1), Par.unit(1))(es).get must beTrue
    Par.equals(Par.unit(1), Par.unit(2))(es).get must beFalse
  }

  // "deadlockFork" in new context {
  //   Par.deadlockFork(5) // hangs for any number
  //   success
  // }

  "delay" in new context {
    var completed = false
    val par = Par.delay {
      Par.unit {
        completed = true
        5
      }
    }

    completed must beFalse
    par(es).get must_== 5
    completed must beTrue
  }

  "choiceBool" in new context {
    Par.choiceBool(Par.unit(true))(Par.unit(1), Par.unit(2))(es).get must_== 1
    Par.choiceBool(Par.unit(false))(Par.unit(1), Par.unit(2))(es).get must_== 2

    Par.choiceBool2(Par.unit(true))(Par.unit(1), Par.unit(2))(es).get must_== 1
    Par.choiceBool2(Par.unit(false))(Par.unit(1), Par.unit(2))(es).get must_== 2
  }

  "choiceN" in new context {
    Par.choiceNList(Par.unit(0))(List(Par.unit(0), Par.unit(1), Par.unit(2)))(es).get must_== 0
    Par.choiceNList(Par.unit(1))(List(Par.unit(0), Par.unit(1), Par.unit(2)))(es).get must_== 1
    Par.choiceNList(Par.unit(2))(List(Par.unit(0), Par.unit(1), Par.unit(2)))(es).get must_== 2

    Par.choiceN(Par.unit(0))(List(Par.unit(0), Par.unit(1), Par.unit(2)))(es).get must_== 0
    Par.choiceN(Par.unit(1))(List(Par.unit(0), Par.unit(1), Par.unit(2)))(es).get must_== 1
    Par.choiceN(Par.unit(2))(List(Par.unit(0), Par.unit(1), Par.unit(2)))(es).get must_== 2
  }

  "choiceMap" in new context {
    val map = Map(0 -> Par.unit("zero"), 1 -> Par.unit("one"))
    Par.choiceMap(Par.unit(0))(map)(es).get must_== "zero"
    Par.choiceMap(Par.unit(1))(map)(es).get must_== "one"
  }

  "chooser/flatMap" in new context {
    Par.chooser(Par.unit(1))(Par.unit(_))(es).get must_== 1
    Par.chooser(Par.unit(2))(Par.unit(_))(es).get must_== 2
    Par.chooser(Par.unit(3))(Par.unit(_))(es).get must_== 3

    Par.flatMap(Par.unit(1))(Par.unit(_))(es).get must_== 1
    Par.flatMap(Par.unit(2))(Par.unit(_))(es).get must_== 2
    Par.flatMap(Par.unit(3))(Par.unit(_))(es).get must_== 3

    Par.flatMapViaJoin(Par.unit(1))(Par.unit(_))(es).get must_== 1
    Par.flatMapViaJoin(Par.unit(2))(Par.unit(_))(es).get must_== 2
    Par.flatMapViaJoin(Par.unit(3))(Par.unit(_))(es).get must_== 3
  }

  "join" in new context {
    Par.join(Par.unit(Par.unit(1)))(es).get must_== 1
    Par.join(Par.unit(Par.unit(2)))(es).get must_== 2

    Par.joinViaFlatMap(Par.unit(Par.unit(1)))(es).get must_== 1
    Par.joinViaFlatMap(Par.unit(Par.unit(2)))(es).get must_== 2
  }

  trait context extends Scope with After {
    lazy val es = Executors.newFixedThreadPool(25)
    def after() = es.shutdownNow()

    val ms = TimeUnit.MILLISECONDS

    def blockThread[A](a: A, blockForMillis: Int = 1000) = {
      Thread.sleep(1000)
      a
    }
  }
}
