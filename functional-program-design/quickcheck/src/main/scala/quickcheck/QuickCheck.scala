package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      x <- arbitrary[Int]
      h <- genHeap
    } yield insert(x, h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == Math.min(a, b)
  }

  property("min3") = forAll { (_h: H, _j: H, a: Int, b: Int) =>
    val h = insert(a, _h)
    val j = insert(b, _j)
    val m = findMin(meld(h, j))
    m == findMin(h) || m == findMin(j)
  }

  property("meld with empty") = forAll { (h: H) =>
    h == meld(h, empty) && h == meld(empty, h)
  }

  property("meld with empty does not change min") = forAll { (_h: H, a: Int) =>
    val h = insert(a, _h)
    val m = findMin(h)
    val left = meld(h, empty)
    val right = meld(empty, h)
    m == findMin(left) && m == findMin(right)
  }

  property("empty") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("fold") = forAll { (l: List[Int]) =>
    def iterFill(h: H, l: List[Int]): H = l match {
      case head :: next => iterFill(insert(head, h), next)
      case Nil          => h
    }
    def iterEmpty(l: List[Int], h: H): List[Int] =
      if (isEmpty(h)) l
      else iterEmpty(l :+ findMin(h), deleteMin(h))

    val full = iterFill(empty, l)
    iterEmpty(Nil, full) == l.sorted
  }
}
