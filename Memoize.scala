case class Memoize1[-T, +R](f: T => R) extends (T => R) {
  private[this] val knownVals = collection.mutable.Map[T, R]()
  def apply(x: T): R = knownVals.getOrElseUpdate(x, f(x))
}