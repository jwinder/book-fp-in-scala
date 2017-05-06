package concurrent

import data._
import org.specs2.mutable.Specification
import org.specs2.specification.{Scope, After}
import java.util.concurrent.{Executors, TimeUnit}

class NonblockingParSpec extends Specification {

  val npar = NonblockingPar

  "unit" in new context {
    npar.run(es)(npar.unit(5)) must_== 5
  }

  "fork & run" in new context {
    var ran = false
    val par = npar.fork {
      npar.unit {
        ran = true
        5
      }
    }
    ran must beFalse
    npar.run(es)(par) must_== 5
    ran must beTrue
  }

  "map2" in new context {
    val par = npar.map2(npar.unit(5), npar.unit(2))(_*_)
    npar.run(es)(par) must_== 10
  }

  "runSafe" in new context {
    npar.runSafe(es)(npar.unit(1)) must_== Right(1)

    // can't get this to work
    // npar.runSafe(es)(npar.unit(throw new RuntimeException)) must_== Left(new RuntimeException)
  }

  "equals" in new context {
    npar.run(es)(npar.equals(npar.unit(1), npar.unit(1))) must beTrue
    npar.run(es)(npar.equals(npar.unit(1), npar.unit(2))) must beFalse
  }

  "sequence" in new context {
    npar.run(es)(npar.sequence(List(npar.unit(1), npar.unit(2), npar.unit(3)))) must_== List(1,2,3)
  }

  "map" in new context {
    npar.run(es)(npar.map(npar.unit(List(1,2,3,4,5)))(_.size)) must_== 5
  }

  "flatMap" in new context {
    npar.run(es)(npar.flatMap(npar.unit(1))(npar.unit(_))) must_== 1
    npar.run(es)(npar.flatMap(npar.unit(2))(npar.unit(_))) must_== 2
    npar.run(es)(npar.flatMap(npar.unit(3))(npar.unit(_))) must_== 3
  }

  "join" in new context {
    npar.run(es)(npar.join(npar.unit(npar.unit(1)))) must_== 1
    npar.run(es)(npar.join(npar.unit(npar.unit(2)))) must_== 2
  }

  trait context extends Scope with After {
    lazy val es = Executors.newFixedThreadPool(25)
    def after() = es.shutdownNow()
  }
}
