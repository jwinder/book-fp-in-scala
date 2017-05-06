package chapters

object Chapter7 {
  // Par exercises are implemented in concurrent.Par
  // NonblockingPar exercises are implemented in concurrent.NonblockingPar

  // law of mapping
  // map(unit(x))(f) == unit(f(x))
  // map(unit(x))(id) == unit(id(x))
  // map(unit(x)) == unit(x)
  // map(y)(id) == y

  // exercise 7.7
  // show that map(map(y)(g))(f) == map(y)(f compose g)
  // using definition (f compose g)(x) := f(g(x))
  // using definition map(x)(f) := f(x)
  // map(map(y)(g))(f) ==> map(g(y))(f) ==> f(g(y)) ==> (f compose g)(y)

  // law of forking
  // fork(x) == x
}
