package james.graph

import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

import james.linalg.Mtx

abstract class GraphComponent {

	val children = new ListBuffer[GraphComponent]()
	val parents = new ListBuffer[GraphComponent]()
	val dim: (Option[Int], Option[Int])
	

	val name: String
	var hasCalcDeriv: Boolean = false
	var currentReference = 0

	private var maxCached = 1;
	private var cachedDerivInner = new ListBuffer[Mtx]()
	private var cachedOutputInner = new ListBuffer[Mtx]()

	def setMaxCached(num: Int): Unit = {maxCached = num}

	def addDependent(dep: GraphComponent): Unit = {
		children += dep
		dep.parents += this
	}

	def adjustInner(eta: Double, past: Int, from: Int): Unit = {
		parents.foreach(_.adjustInner(eta, past, from))
	}
	def adjust(eta: Double): Unit = {
		adjustInner(eta, 0, 0)
	}

	def cachedDeriv(num: Int): Mtx = cachedDerivInner(num)

	def derivInner(time: Int, from: Int): Mtx = {
		if( (hasCalcDeriv && time == 0 && from == 0) ){
			cachedDeriv(time)
		}else{
			val filteredChildren = if (time == 0) children else children.filter(_.isInstanceOf[PastMtx])
			val ds = children.map(_.derivWRT(this))
			val added = ds.tail.fold(ds.head)(_.add(_))
			cachedDerivInner.prepend(added)
			if(cachedDerivInner.length > maxCached){
				cachedDerivInner.remove(cachedDerivInner.length-1)
			}
			hasCalcDeriv = true
			cachedDerivInner(0)
		}
	}
	def deriv(): Mtx = {
		derivInner(0,0)
	}

	def run(a: Map[String, Mtx]): Mtx = output(a)
	def cachedOutput(num: Int): Mtx = cachedOutputInner(0)
	def output(a: Map[String, Mtx]): Mtx = {
		hasCalcDeriv = false
		cachedOutputInner.prepend(outputInner(a))
		if (cachedOutputInner.length > maxCached){
			cachedOutputInner.remove(cachedOutputInner.length-1)
		}
		cachedOutputInner(0)
	}
	
	//NEED IMPLEMENTATIONS OF THESE IN EACH
	def abstractOutput(): (Option[Int], Option[Int])
	def outputInner(a: Map[String, Mtx]): Mtx
	def derivWRT(wrt: GraphComponent): Mtx 
	
}