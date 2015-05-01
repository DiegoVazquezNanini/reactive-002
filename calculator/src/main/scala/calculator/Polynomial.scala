package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b()*b() - 4 * a() * c())
    //b² - 4ac
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val delta = computeDelta(a, b, c)()
    val positive = (-b() + delta)/ a() * 2
    val negative = (-b() - delta)/ a() * 2
      Signal(Set(positive, negative))
    //(-b ± √Δ) / (2a)
  }
}
