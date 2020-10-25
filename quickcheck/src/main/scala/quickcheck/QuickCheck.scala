package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    h <- oneOf[H](empty, genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("heap is empty after delete the only element") = forAll { x: Int =>
    val h = insert(x, empty)
    isEmpty(deleteMin(h))
  }

  property("findMin works properly") = forAll {(a: Int, b: Int, c: Int) =>
    import Math._
    val minVal = min(min(a, b), c)
    val heap = insert(c, insert(b, insert(a, empty)))
    findMin(heap) == minVal
  }

  property("min before meld and after is the same") = forAll { (a: H, b: H) =>
    import Math._
      min(findMin(a), findMin(b)) == findMin(meld(a, b))
  }

  property("delete min for melded should remove the same elem") = forAll {(a: H, b: H) =>
    findMin(if (findMin(a) < findMin(b)) meld(deleteMin(a), b) else meld(deleteMin(b),a)) == findMin(deleteMin(meld(a, b)))
  }
}
