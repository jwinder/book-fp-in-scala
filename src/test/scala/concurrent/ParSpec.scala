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

  trait context extends Scope with After {
    lazy val es = Executors.newSingleThreadExecutor()
    def after() = es.shutdownNow()

    val ms = TimeUnit.MILLISECONDS

    def blockThread[A](a: A, blockForMillis: Int = 1000) = {
      Thread.sleep(1000)
      a
    }
  }
}
