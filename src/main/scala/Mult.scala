package james.graph

import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

import james.linalg.Mtx

class Mult(
		val name: String,
		first: GraphComponent,
		second: GraphComponent
	) extends GraphComponent {

		first.addDependent(this)
		second.addDependent(this)

		val dim = (first.abstractOutput()._1, second.abstractOutput()._2)

		override def abstractOutput(): (Option[Int], Option[Int]) = dim
		override def outputInner(inputs: Map[String, Mtx]): Mtx = {
			first.output(inputs) * second.output(inputs)
		}

		override def derivWRT(wrt: GraphComponent): Mtx = {
			if (wrt.eq(first)){
				val n = this.deriv()
				n.mult(second.cachedOutput(0).trans())
			}else if (wrt.eq(second)){
				var n = first.cachedOutput(0).trans()
				if (n.colNum != this.deriv().rowNum){
					n = n.foldToSingleCol()
				}
				n.mult(this.deriv())
			} else {
				Mtx.filledMtx(wrt.dim._1.get, wrt.dim._2.get, 0)
			}
		}
}