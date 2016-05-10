package james.graph

import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

import james.linalg.Mtx

class InputMtx (
		val name: String,
		val dim: (Option[Int], Option[Int])
	) extends GraphComponent {
		override def abstractOutput(): (Option[Int], Option[Int]) = dim
		override def outputInner(inputs: Map[String, Mtx]): Mtx = inputs(name)
		override def derivWRT(wrt: GraphComponent): Mtx = {
			Mtx.filledMtx(wrt.dim._1.get, wrt.dim._2.get, 0)
		}
}