package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(
      a: Signal[Double],
      b: Signal[Double],
      c: Signal[Double]
  ): Signal[Double] =
    Signal {
      val a_ = a()
      val b_ = b()
      val c_ = c()
      b_ * b_ - 4 * a_ * c_
    }

  def computeSolutions(
      a: Signal[Double],
      b: Signal[Double],
      c: Signal[Double],
      delta: Signal[Double]
  ): Signal[Set[Double]] =
    Signal {
      val delta_ = delta()
      if (delta_ < 0) Set()
      else {
        val a_ = a()
        val b_ = b()
        val res1 = (-b_ + Math.sqrt(delta_)) / (2 * a_)
        val res2 = (-b_ - Math.sqrt(delta_)) / (2 * a_)
        Set(res1, res2)
      }
    }
}
