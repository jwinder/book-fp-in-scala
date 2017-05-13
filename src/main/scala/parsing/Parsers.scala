package parsing

import scala.util.matching.Regex
import data._

trait Parsers[ParseError, Parser[+_]] { self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def string(s: String): Parser[String]
  implicit def parserOps[A](p: Parser[A]) = new ParserOps[A](p)
  implicit def asParserOps[A,B](a: A)(implicit f: A => Parser[B]): ParserOps[B] = new ParserOps[B](f(a))

  implicit def regex(r: Regex): Parser[String]

  implicit def char(c: Char): Parser[Char] = string(c.toString).map(_ charAt 0)

  def map[A,B](a: Parser[A])(f: A => B): Parser[B]
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  // succeed(a).run(s) == Right(a)
  def succeed[A](a: A): Parser[A]
  def succeed1[A](a: A): Parser[A] = string("").map(_ => a)

  // (string("abtra") or string("cadabra")).run(abra)  == Right("abra")
  // (string("abtra") or string("cadabra")).run(cadabra)  == Right("cadabra")
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  // slice('a' | 'b').many.run("aaba") == Right("aaba")
  // meant to return the values but ignore the list creation used in `many`
  def slice[A](p: Parser[A]): Parser[String]

  // exercise 9.1
  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = (p ** p2).map(f.tupled)
  def many1[A](p: Parser[A]): Parser[List[A]] = p.map2(many(p))(Cons.apply)

  // exercise 9.3, implement many in terms of or, map2 & succeed
  // char('a').many.run("aaa") == Right(List('a', 'a', 'a'))
  def many[A](p: Parser[A]): Parser[List[A]] = p.map2(many(p))(Cons.apply) or succeed(Nil)

  // exercise 9.4
  // listOfN(3, "ab" | "cad").run("ababcad") == Right("ababcad")
  // listOfN(3, "ab" | "cad").run("cadabab") == Right("cadabab")
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n > 0) {
      p.map2(listOfN(n-1, p))(Cons.apply)
    } else {
      succeed(Nil)
    }

  // exercise 9.5
  // I like using a separate combinator so the laziness is more explicit. Parameters don't have to be call-by-name.

  // exercise 9.6
  // valid inputs: "0", "1a", "2aa", "3aaa", etc.
  // e.g. digitAndChars.run("2aa") == Right(List('a', 'a'))
  def digitAndChars: Parser[List[Char]] = "[0-9]+".r.flatMap { d => listOfN(d.toInt, 'a') }

  // exercise 9.7, product & map2 via flatMap

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] = ???
  def map2ViaFlatmap[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = ???

  class ParserOps[A](p: Parser[A]) {
    def run(input: String): Either[ParseError, A] = self.run(p)(input)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def product[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def **[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def map2[B,C](p2: => Parser[B])(f: (A,B) => C): Parser[C] = self.map2(p, p2)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def many: Parser[List[A]] = self.many(p)
    def many1: Parser[List[A]] = self.many1(p)
    def slice: Parser[String] = self.slice(p)
  }

  object ParserLaws {
    import check._

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = {
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))
    }

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = {
      equal(p, p.map(identity))(in)
    }

    // exercise 9.2
    def productLaw[A,B](p: Parser[A], p2: Parser[B])(in: Gen[String]): Prop = {
      Prop.forAll(in) { s =>
        val result = for {
          a <- run(p)(s)
          b <- run(p2)(s)
        } yield (a,b)
        result == run(p ** p2)(s)
      }
    }
  }
}
