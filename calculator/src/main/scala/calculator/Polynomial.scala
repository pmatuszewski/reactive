package calculator

import math._

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    new Var[Double](pow(b(), 2) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    new Var(
      if(delta() < 0)
        Set[Double]()
      else if(delta() == 0)
        Set[Double](-b() / (2 * a()))
      else
        Set[Double]((-b() - sqrt(delta())) / (2 * a()), (-b() + sqrt(delta())) / (2 * a()))
    )

  }
}
