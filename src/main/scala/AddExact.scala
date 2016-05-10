package james.graph

import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

import james.linalg.Mtx

class AddExact(
		val name: String,
		first: GraphComponent,
		second: GraphComponent
	) extends GraphComponent {

		first.addDependent(this)
		second.addDependent(this)
		val dim = first.dim
		override def abstractOutput(): (Option[Int], Option[Int]) = dim
		override def outputInner(inputs: Map[String, Mtx]): Mtx = {
			first.output(inputs) + second.output(inputs)
		}
		override def derivWRT(wrt: GraphComponent): Mtx = {
			if (wrt.eq(first) || wrt.eq(second)){
				this.deriv()
			} else {
				Mtx.filledMtx(wrt.dim._1.get, wrt.dim._2.get, 0)
			}
		}
}