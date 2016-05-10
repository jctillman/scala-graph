package james.graph

import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

import james.linalg.Mtx

class VariableMtx(
		val name: String,
		val dim: (Option[Int], Option[Int]),
		fill: (Int, Int) => Double
	) extends GraphComponent {
		var value = Mtx.filledFuncMtx(dim._1.get, dim._2.get, fill)
		override def abstractOutput(): (Option[Int], Option[Int]) = dim
		override def outputInner(inputs: Map[String, Mtx]): Mtx = value
		override def derivWRT(wrt: GraphComponent): Mtx = {
			Mtx.filledMtx(wrt.dim._1.get, wrt.dim._2.get, 0)
		}
		override def adjustInner(eta: Double, past: Int): Unit = {
			value -= this.deriv().scalarMult(eta)
		}
}