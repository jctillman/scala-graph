package james.graph

import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

import james.linalg.Mtx

class Add(
		val name: String,
		first: GraphComponent,
		second: GraphComponent
	) extends GraphComponent {

		first.addDependent(this)
		second.addDependent(this)
		val dim = if (first.dim._1.isEmpty || first.dim._2.isEmpty){
			second.dim
		}else{
			first.dim
		}
		
		override def abstractOutput(): (Option[Int], Option[Int]) = dim
		override def outputInner(inputs: Map[String, Mtx]): Mtx = {
			val firOut = first.output(inputs)
			val secOut = second.output(inputs)

			if (firOut.rowNum == 1 && secOut.rowNum > 1){
				secOut.broadcastRow(firOut)
			}else if (secOut.rowNum == 1 && firOut.rowNum > 1){
				firOut.broadcastRow(secOut)

			}else if (firOut.colNum == 1 && secOut.colNum > 1){
				secOut.broadcastCol(firOut)
			}else if (secOut.colNum == 1 && firOut.colNum > 1){
				firOut.broadcastCol(secOut)

			}else{
				firOut + secOut
			}
		}
		
		override def derivWRT(wrt: GraphComponent): Mtx = {

			def proc(a: GraphComponent): Mtx = {
				if(!a.dim._1.isEmpty && a.dim._1.get == 1){
					this.deriv().foldToSingleRow()
				}else if (!a.dim._2.isEmpty && a.dim._2.get == 1){
					this.deriv().foldToSingleCol()
				}else{
					this.deriv()
				}
			}

			if (wrt.eq(first)){
				proc(first)
			}else if (wrt.eq(second)){
				proc(second)
			}else {
				Mtx.filledMtx(wrt.dim._1.get, wrt.dim._2.get, 0)
			}
		}
}