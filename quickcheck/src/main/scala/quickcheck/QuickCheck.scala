package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

	val integers = scala.util.Random.nextInt()




			lazy val nonEmptyheap: Gen[H]  = for
			{
				int <- arbitrary[Int]	
						b <- arbitrary[Boolean]
								heap <- if(b) const(empty) else nonEmptyheap 
			}yield insert(int,heap)


			lazy val genHeap: Gen[H] = oneOf(const(empty),nonEmptyheap,nonEmptyheap)


			implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

			property("gen1") = forAll { (h: H) =>
			val m = if (isEmpty(h)) 0 else findMin(h)
			findMin(insert(m, h)) == m
			}

			property("empty") = forAll{ a:Int =>
			val heap = insert(a,empty)
			findMin(heap) == a

			}
			property("Delempty") = forAll{ a:Int =>
			val heap = deleteMin(insert(a,empty))
			heap == empty

			}

			property ("heapMins") = forAll{(h1:H,h2:H) =>
			val m1 = if (isEmpty(h1)) 0 else findMin(h1)
			val m2 = if (isEmpty(h2)) 0 else findMin(h2)
			//
			val m3 = if (isEmpty(h2) && isEmpty(h1)) 0 
			else if(isEmpty(h2)) findMin(h1)
			else if(isEmpty(h1)) findMin(h2)
			else findMin(meld(h1,h2))

			if(!(isEmpty(h2) || isEmpty(h1)))
				if(m1 < m2){
					m3 == m1

				}
				else { 
					m3 == m2
				}
			else{
				if(isEmpty(h2))
					m3==m1
					else m3==m2
			}

			}

			property ("deleteMin") = forAll{ (h:H) =>

			if(!isEmpty(h))
			{
				val m = findMin(h) 
						val h2 = deleteMin(h)
						if(!isEmpty(h2))
						{
							m <= findMin(h2)
						}else h2 == empty
			}else h == empty

			}


			property ("order") = forAll{(h:H) =>
			val oh = List()
			val (h1,h2) = delMin(h,oh)
			h2 == h2.sorted
			}

			property("meldemin")= forAll{ (h:H) =>

			if(h != empty) findMin(h) == findMin(meld(h,h))
			else h == empty


			}
			property ("doubleins")= forAll{(h:H)=>
			if(h != empty){
				val h2= insert(findMin(h),h)
						val h3 =insert(findMin(h),h2)
						val h4 = deleteMin(h3)
						findMin(h4) == findMin(h)

			}else 
				h == empty

			}
			property ("doubledel")= forAll{(h:H)=>
			if(h != empty){
				val h2= insert(findMin(h),h)
						val h3 =insert(findMin(h),h2)
						val h4 = deleteMin(h3)
						val h5 = deleteMin(h4)
						findMin(h5) >= findMin(h)

			}else 
				h == empty

			}

			property ("breakdown") = forAll{(h:H) =>
			if(h!=empty){

				val e = insert(findMin(h),empty)
						val m = meld(e,deleteMin(h))
						findMin(m) == findMin(h)
			}
			else h == empty  


			}

			property ("meld") = forAll{(h1:H,h2:H) =>
			val m1 = if (isEmpty(h1)) 0 else findMin(h1)
			val m2 = if (isEmpty(h2)) 0 else findMin(h2)
			//
			val m3 = meld(h1,h2)

			if(!(isEmpty(h2) || isEmpty(h1)))
				if(m1 < m2){
					val m4 = meld(insert(m1,h2),h1)
							val m5 = deleteMin(m4)
							findMin(m5) == m1 
				}
				else { 
					val m4 = meld(insert(m2,h1),h2)
							val m5 = deleteMin(m4)
							findMin(m5) == m2
				}
			else{
				if(isEmpty(h2))
					h2 == empty
					else h1 == empty
			}

			}

			property("insertList") = forAll { list: List[Int] =>
			    def insertList(list: List[Int], h: H): H = list match {
			      case Nil => h
			      case t :: ts => insertList(ts, insert(t, h))
			    }
			    val oh = List()
					val (listo,h1) = delMin(insertList(list,empty),oh)
					list.sorted == (h1)
			}

}
