
package james.util


import scala.util.{Random => r}
import scala.math.sqrt
import scala.math.log
import scala.math.Pi
import scala.math.sin

object misc {

	def transpose[A](lst: List[List[A]]): List[List[A]] = {
		lst(0).zipWithIndex.map(
			(a: (A,Int)) => lst.zipWithIndex.map(
				(b: (List[A], Int)) => lst(b._2)(a._2)))
	}

	def normal(stdDev: Double): Double = {
		val one = r.nextDouble
		val two = r.nextDouble
		sqrt(-2.0 * log(one)) * sin(2 * two * Pi) * stdDev
	}

}