package data

case class State[S,+A](run: S => (A,S)) {
  import State._

  // exercise 6.10

  def flatMap[B](f: A => State[S,B]): State[S,B] = State { state =>
    val (a,s) = run(state)
    f(a).run(s)
  }

  def map[B](f: A => B): State[S,B] = flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S,B])(f: (A,B) => C): State[S,C] = {
    flatMap(a => sb.map(b => f(a,b)))
  }
}

object State {

  // exercise 6.10

  def unit[S,A](a: A): State[S,A] = State((a, _))

  def sequence[S,A](states: List[State[S,A]]): State[S, List[A]] = State { s =>
    val (list, finalS) = states.foldLeft((List.empty[A], s)) {
      case ((as, lastS), state) =>
        val (nextA, nextS) = state.run(lastS)
        (Cons(nextA, as), nextS)
    }
    (list.reverse, finalS)
  }

  def get[S]: State[S, S] = State(s => (s,s))

  def set[S](s: S): State[S, Unit] = State(_ => ((),s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}
