package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a:Int, b:Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == List(a, b).min
  }

  property("empty") = forAll { a:Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  def getAll(h: H): List[A] = {
    if(isEmpty(h)) Nil else findMin(h) :: getAll(deleteMin(h))
  }

  property("sorted") = forAll { h:H =>
    val l = getAll(h)
    l == l.sorted
  }

  property("melding") = forAll { (h1:H, h2:H) =>
    findMin(meld(h1, h2)) == List(findMin(h1), findMin(h2)).min
  }

  def putAll(l:List[A]): H = l match {
    case h::t => insert(h, putAll(t))
    case Nil => empty
  }

  property("elems") = forAll{ l:List[A] =>
    val h = putAll(l)
    getAll(h) == l.sorted
  }

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
