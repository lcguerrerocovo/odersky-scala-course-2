package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
      Signal(Math.pow(b(),2) - 4*(a() * c()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    def root(delta: Signal[Double],f: (Double,Double) => Double): Double = (f(-b(),Math.sqrt(delta()))) / (2d * a())

    if(delta() < 0d) Signal(Set())
    else if(delta() == 0d) Signal(Set(root(delta,_ + _)))
    else  Signal(Set(root(delta,_ - _),root(delta,_ + _)))
  }
}
