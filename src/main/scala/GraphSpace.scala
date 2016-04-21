package james.graph

import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

import james.linalg.Mtx
import james.linalg.Vct

import james.graph.funcs.DerivFunc
import james.graph.funcs.Differentiable


abstract class GraphComponent {

	val children = new ListBuffer[GraphComponent]()
	val parents = new ListBuffer[GraphComponent]()
	val dimensions: (Option[Int], Option[Int])

	val name: String
	var hasCalculatedDerivative: Boolean = false;
	var cachedConcreteDerivative: Mtx = Mtx.filledMtx(1,1,11)
	var cachedConcreteOutput: Mtx = Mtx.filledMtx(1,1,11)

	def abstractOutput(): (Option[Int], Option[Int])
	def addDependent(dep: GraphComponent): Unit = {
		children += dep
		dep.parents += this
	}
	def adjust(eta: Double): Unit = {
		parents.foreach(_.adjust(eta))
	}

	def concreteDerivative(): Mtx = {
		if(hasCalculatedDerivative){
			cachedConcreteDerivative
		}else{
			if (children.length == 1){
				cachedConcreteDerivative = children(0).concreteDerivativeWithRespectTo(this)
				hasCalculatedDerivative = true
				cachedConcreteDerivative

			}else{
				val derived = children.map(_.concreteDerivativeWithRespectTo(this))
				cachedConcreteDerivative = derived.tail.foldLeft(derived.head)( _.add(_) )
				hasCalculatedDerivative = true
				cachedConcreteDerivative
			}
		}
	}
	def concreteOutputInner(a: Map[String, Mtx]): Mtx
	def concreteOutput(a: Map[String, Mtx]): Mtx = {
		hasCalculatedDerivative = false
		cachedConcreteOutput = concreteOutputInner(a)
		cachedConcreteOutput
	}

	def concreteDerivativeWithRespectTo(wrt: GraphComponent): Mtx 
	def run(a: Map[String, Mtx]): Mtx = concreteOutput(a)
}

class InputMtx (
		val name: String,
		val dimensions: (Option[Int], Option[Int])
	) extends GraphComponent {
		override def concreteOutputInner(inputs: Map[String, Mtx]): Mtx = {
			inputs(name)
		}
		override def abstractOutput(): (Option[Int], Option[Int]) = dimensions
		override def concreteDerivativeWithRespectTo(wrt: GraphComponent): Mtx = {
			Mtx.filledMtx(wrt.dimensions._1.get, wrt.dimensions._2.get, 0)
		}
}

class VariableMtx(
		val name: String,
		val dimensions: (Option[Int], Option[Int]),
		fill: (Int, Int) => Double
	) extends GraphComponent {
		var value = Mtx.filledFuncMtx(dimensions._1.get, dimensions._2.get, fill)
		override def concreteOutputInner(inputs: Map[String, Mtx]): Mtx = {
			value
		}
		override def abstractOutput(): (Option[Int], Option[Int]) = dimensions
		override def concreteDerivativeWithRespectTo(wrt: GraphComponent): Mtx = {
			Mtx.filledMtx(wrt.dimensions._1.get, wrt.dimensions._2.get, 0)
		}
		override def adjust(eta: Double): Unit = {
			val derivative = this.concreteDerivative()
			val changed = derivative.scalarMult(eta)
			value = value - changed
		}
}

class Add(
		val name: String,
		first: GraphComponent,
		second: GraphComponent
	) extends GraphComponent {

		//assert(first.abstractOutput() == second.abstractOutput())

		first.addDependent(this)
		second.addDependent(this)
		val dimensions = if (first.dimensions._1.isEmpty || first.dimensions._2.isEmpty){
			second.dimensions
		}else{
			first.dimensions
		}
		
		override def concreteOutputInner(inputs: Map[String, Mtx]): Mtx = {
			val firOut = first.concreteOutput(inputs)
			val secOut = second.concreteOutput(inputs)

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
		override def abstractOutput(): (Option[Int], Option[Int]) = first.abstractOutput()
		override def concreteDerivativeWithRespectTo(wrt: GraphComponent): Mtx = {
			if (wrt.eq(first)){

				if(!first.dimensions._1.isEmpty && first.dimensions._1.get == 1){
					this.concreteDerivative().foldToSingleRow()
				}else if (!first.dimensions._2.isEmpty && first.dimensions._2.get == 1){
					this.concreteDerivative().foldToSingleCol()
				}else{
					this.concreteDerivative()
				}

			}else if (wrt.eq(second)){

				if(!second.dimensions._1.isEmpty && second.dimensions._1.get == 1){
					this.concreteDerivative().foldToSingleRow()
				}else if (!second.dimensions._2.isEmpty && second.dimensions._2.get == 1){
					this.concreteDerivative().foldToSingleCol()
				}else{
					this.concreteDerivative()
				}

			} else {
				Mtx.filledMtx(wrt.dimensions._1.get, wrt.dimensions._2.get, 0)
			}
		}
}

class AddExact(
		val name: String,
		first: GraphComponent,
		second: GraphComponent
	) extends GraphComponent {

		//assert(first.abstractOutput() == second.abstractOutput())

		first.addDependent(this)
		second.addDependent(this)
		val dimensions = first.dimensions
		
		override def concreteOutputInner(inputs: Map[String, Mtx]): Mtx = {
			cachedConcreteOutput = first.concreteOutput(inputs) + second.concreteOutput(inputs)
			cachedConcreteOutput
		}
		override def abstractOutput(): (Option[Int], Option[Int]) = first.abstractOutput()
		override def concreteDerivativeWithRespectTo(wrt: GraphComponent): Mtx = {
			if (wrt.eq(first)){
				this.concreteDerivative()
			}else if (wrt.eq(second)){
				this.concreteDerivative()
			} else {
				Mtx.filledMtx(wrt.dimensions._1.get, wrt.dimensions._2.get, 0)
			}
		}
}

class Mult(
		val name: String,
		first: GraphComponent,
		second: GraphComponent
	) extends GraphComponent {

		first.addDependent(this)
		second.addDependent(this)
		//assert(first.abstractOutput()._2 == second.abstractOutput()._1)
		val dimensions = (first.abstractOutput()._1, second.abstractOutput()._2)

		override def concreteOutputInner(inputs: Map[String, Mtx]): Mtx = {
			first.concreteOutput(inputs) * second.concreteOutput(inputs)
		}

		override def abstractOutput(): (Option[Int], Option[Int]) = {
			(first.abstractOutput()._1, second.abstractOutput()._2)
		}
		override def concreteDerivativeWithRespectTo(wrt: GraphComponent): Mtx = {
			if (wrt.eq(first)){
				val n = this.concreteDerivative()
				val temp = n.mult(second.cachedConcreteOutput.trans())
				temp
			}else if (wrt.eq(second)){
				val n = first.cachedConcreteOutput.trans().mult(this.concreteDerivative())
				n
			} else {
				Mtx.filledMtx(wrt.dimensions._1.get, wrt.dimensions._2.get, 0)
			}
		}
}

class Transform(
	val name: String,
	first: GraphComponent,
	func: DerivFunc) extends GraphComponent{

	first.addDependent(this)

	val dimensions = (first.abstractOutput()._1, first.abstractOutput()._2)
	override def abstractOutput(): (Option[Int], Option[Int]) = {
		(first.abstractOutput()._1, first.abstractOutput()._2)
	}
	override def concreteOutputInner(inputs: Map[String, Mtx]): Mtx = {
		first.concreteOutput(inputs).piecewise(func.result)
	}
	override def concreteDerivativeWithRespectTo(wrt: GraphComponent): Mtx = {
		if(wrt.eq(first)){
			this.cachedConcreteOutput.piecewise(func.derive).hadamard(this.concreteDerivative())
		}else{
			Mtx.filledMtx(wrt.dimensions._1.get, wrt.dimensions._2.get, 0)
		}
	}

}

class MeanSquaredLoss(
	val name: String,
	first: GraphComponent,
	second: GraphComponent
	) extends GraphComponent {

		first.addDependent(this)
		val dimensions = (first.abstractOutput()._1, first.abstractOutput()._2)
		override def abstractOutput() = first.abstractOutput()
		override def concreteOutputInner(inputs: Map[String, Mtx]): Mtx = {
			val unadjusted = (first.concreteOutput(inputs) - second.concreteOutput(inputs)).frobenius()
			val adjusted = unadjusted / first.cachedConcreteOutput.colNum
			new Mtx(List(List(unadjusted)))
		}
		override def concreteDerivativeWithRespectTo(wrt: GraphComponent): Mtx = {
			if(wrt.eq(first)){
				val diff = first.cachedConcreteOutput - second.cachedConcreteOutput
				diff
			}else if(wrt.eq(second)){
				val diff = second.cachedConcreteOutput - first.cachedConcreteOutput
				diff
			}else{
				Mtx.filledMtx(wrt.dimensions._1.get, wrt.dimensions._2.get, 0)
			}
		}
}




