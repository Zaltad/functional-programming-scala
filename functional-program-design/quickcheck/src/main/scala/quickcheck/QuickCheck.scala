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
      h <- oneOf(const(empty), genHeap)
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
    val h = insert(a, empty)
    findMin(insert(b, h)) == Math.min(a, b)
  }

  property("insDel") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("sort") = forAll { (h: H) =>
    if (isEmpty(h)) {
      true
    } else {
      val mini = findMin(h)
      check(mini, deleteMin(h))
    }
  }

  def check(lastMin: Int, h: H): Boolean = {
    if (isEmpty(h)) {
      true
    } else {
      val mini = findMin(h)
      if (lastMin > mini) {
        false
      } else {
        check(mini, deleteMin(h))
      }
    }
  }

  property("meldMin") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) && isEmpty(h2)) {
      true
    } else if (isEmpty(h1) || isEmpty(h2)) {
      val meldedMin = findMin(meld(h1, h2))
      val mini = if (isEmpty(h1)) findMin(h2) else findMin(h1)
      meldedMin == mini
    } else {
      val min1 = findMin(h1)
      val min2 = findMin(h2)
      val meldedMin = findMin(meld(h1, h2))
      meldedMin == min1 || meldedMin == min2
    }
  }

}
