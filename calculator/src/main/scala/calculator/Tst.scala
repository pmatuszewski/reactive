package calculator

/**
 * Created by piotr on 4/26/15.
 */
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Polynomial._

class QuickCheckPoly extends Properties("Double") {

  property("delta") = forAll { (a: Double, b:Double, c: Double)  =>
    val delta = computeDelta(Var(a), Var(b), Var(c))
    val roots = computeSolutions(Var(a), Var(b), Var(c), delta)()
    if(delta() < 0)
      roots == Set()
    else if (delta() == 0)
      roots == Set(-b/(2*a))
    else if (delta().isInfinity || delta().isNaN)
      true
    else {
      val error = (roots.sum + b / a)/(-b/a)
      error.isNaN || error < 1
    }
  }


}
