package james.graph

import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

import james.linalg.Mtx
import james.graph.funcs.DerivFunc

class Transform(
	val name: String,
	first: GraphComponent,
	func: DerivFunc) extends GraphComponent{

	first.addDependent(this)

	val dim = (first.abstractOutput()._1, first.abstractOutput()._2)
	override def abstractOutput(): (Option[Int], Option[Int]) = dim
	override def outputInner(inputs: Map[String, Mtx]): Mtx = {
		first.output(inputs).piecewise(func.result)
	}
	override def derivWRT(wrt: GraphComponent): Mtx = {
		if(wrt.eq(first)){
			this.cachedOutput(0).piecewise(func.derive).hadamard(this.deriv())
		}else{
			Mtx.filledMtx(wrt.dim._1.get, wrt.dim._2.get, 0)
		}
	}

}