
package james.graph

import scala.collection.mutable.Map
import james.linalg.Mtx

class MeanSquaredLoss(
	val name: String,
	first: GraphComponent,
	second: GraphComponent
	) extends GraphComponent {

		first.addDependent(this)
		val dim = (first.abstractOutput()._1, first.abstractOutput()._2)
		override def abstractOutput() = dim
		override def outputInner(inputs: Map[String, Mtx]): Mtx = {
			val unadjusted = (first.output(inputs) - second.output(inputs)).frobenius()
			val adjusted = unadjusted / first.cachedOutput(0).colNum
			new Mtx(List(List(unadjusted)))
		}
		override def derivWRT(wrt: GraphComponent): Mtx = {

			def proc(a: GraphComponent, b: GraphComponent): Mtx = {
				if (a.cachedOutput(0).rowNum == 1){
					(a.cachedOutput(0) - b.cachedOutput(0)).foldToSingleRow()
				} else if (wrt.cachedOutput(0).colNum == 1){
					(a.cachedOutput(0) - b.cachedOutput(0)).foldToSingleCol()
				}
				a.cachedOutput(0) - b.cachedOutput(0)
			}

			if(wrt.eq(first)){
				proc(first, second)
			}else if(wrt.eq(second)){
				proc(second, first)
			}else{
				Mtx.filledMtx(wrt.dim._1.get, wrt.dim._2.get, 0)
			}
		}
}




