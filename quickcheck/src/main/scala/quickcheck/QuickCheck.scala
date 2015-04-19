package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: A =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("hint1") = forAll { (a: A, b: A) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    findMin(h2) == ord.min(a,b)
  }

  property("hint2") = forAll { a: A =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)
    isEmpty(h2)
  }

  property("hint4") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    findMin(h) == findMin(h1) || findMin(h) == findMin(h2)
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h))==m
  }

  property("gen1") = forAll { l: List[Int] =>
    toList(toHeap(l)) == l.sorted
  }

  def toList(h: H): List[Int] = {
    if (isEmpty(h)) Nil
    else findMin(h) :: toList(deleteMin(h))
  }

  def toHeap(l: List[Int]): H = l match {
    case Nil => empty
    case x::xs => insert(x,toHeap(xs))
  }

  lazy val genHeap: Gen[H] = for {
    value <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(value, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
