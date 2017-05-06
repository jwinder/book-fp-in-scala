package check

trait BooleanProp { self =>
  def check: Boolean

  // exercise 8.3
  def &&(p: BooleanProp): BooleanProp = new BooleanProp {
    def check = self.check && p.check
  }
}

object BooleanProp {
  def apply(lazyCheck: => Boolean): BooleanProp = {
    lazy val c = lazyCheck
    new BooleanProp {
      def check = c
    }
  }
}
