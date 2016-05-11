package james.graph

import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

import james.linalg.Mtx

class PastMtx (
	val name: String,
	val past: GraphComponent) extends GraphComponent {

	past.addDependent(this)
	val dim = (past.abstractOutput()._1, past.abstractOutput()._2)	
	def abstractOutput(): (Option[Int], Option[Int]) = past.abstractOutput();
	def outputInner(a: Map[String, Mtx]): Mtx = {
		past.cachedOutput(1)
	}
	def derivWRT(wrt: GraphComponent): Mtx = {
		if (wrt.eq(past)){
			past.cachedDeriv(0)
		} else {
			var dim = wrt.abstractOutput()
			Mtx.filledMtx(dim._1.get, dim._2.get, 0)
		}
	}

	override def adjustInner(eta: Double, time: Int, from: Int): Unit = {
		parents.foreach(_.adjustInner(eta, time+1, from))
	}




	
}