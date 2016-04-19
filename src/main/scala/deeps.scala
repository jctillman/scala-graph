package james.util

object deeps {

	def map[A](a: List[List[A]], b: A => A): List[List[A]] = {
		a.map( x => x.map( y => b(y)))
	}

	def combine[A](a: List[List[A]], b: List[List[A]], c: (A,A) => A): List[List[A]] = {
		assert(a.length == b.length)
		assert(a(0).length == b(0).length)
		a.zip(b).map({ case (x: List[A], y: List[A]) => 
				x.zip(y).map( (a:(A,A)) => c(a._1,a._2) ) 
			})
	}

	def equal[A](a: List[List[A]], b: List[List[A]], c: (A,A) => Boolean): Boolean = {
		if (a.length != b.length) false
		else if (a(0).length != b(0).length) false
		else a.flatten.zip(b.flatten).forall({case (x,y) => c(x,y)})	
	}

	def compare[A](a: List[A], b: (A,A) => Boolean): Boolean = {
		a match {
			case Nil => true
			case _ => a.tail.forall(b(a.head, _)) && compare[A](a.tail, b)
		}
	}

}