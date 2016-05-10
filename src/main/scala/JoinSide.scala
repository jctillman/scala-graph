package james.graph

import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

import james.linalg.Mtx

class JoinSide (
		val name: String,
		val first: GraphComponent,
		val second: GraphComponent
	) extends GraphComponent {

		first.addDependent(this)
		second.addDependent(this)

		assert(first.abstractOutput._1 == second.abstractOutput._1)
		val dim = (first.abstractOutput._1, Option(first.abstractOutput._2.get + second.abstractOutput._2.get))
		override def abstractOutput(): (Option[Int], Option[Int]) = dim
		override def outputInner(inputs: Map[String, Mtx]): Mtx = {
			val f = first.output(inputs)
			val s = second.output(inputs)
			val blank = List[List[Double]]()
			val joined = Mtx.joinList( f.mx, s.mx, blank, blank)
			new Mtx(joined)
		}
		override def derivWRT(wrt: GraphComponent): Mtx = {
			if (wrt.eq(first)){
				val dims = first.abstractOutput()
				val sliceAwayExtraCols = this.deriv().mx.map( row => {
					row.take(dims._2.get)
				})
				new Mtx(sliceAwayExtraCols)
			} else {
				val dims = first.abstractOutput()
				val sliceAwayExtraCols = this.deriv().mx.map( row => {
					row.drop(dims._2.get)
				})
				new Mtx(sliceAwayExtraCols)
			}
		}
}