package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  
  val availableHeaps = List()

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    m <- oneOf(const(empty), genHeap)
  } yield insert(k,m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min of meld") = forAll { (h: H, h2: H) =>
    val h3 = meld(h, h2)
    findMin(h3) == (findMin(h) min findMin(h2))
  }

  property("min of meld after deleting min") = forAll { (h: H, h2: H) =>
    val minh = findMin(h)
    val minh2 = findMin(h2)
    val h3 = meld(h, h2)
    val newh = if (minh <= minh2) meld(deleteMin(h),h2) else meld(h,deleteMin(h2))
    findMin(deleteMin(h3)) == findMin(newh)
  }
}
