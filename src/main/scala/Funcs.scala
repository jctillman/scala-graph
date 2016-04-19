package james.graph.funcs

import scala.math.exp;
import scala.math.tanh;

class DerivFunc(baseResult: Double => Double, baseDerive: Double => Double) {
	def result(input: Double): Double = baseResult(input)
	def derive(input: Double): Double = baseDerive(input)
}

object Differentiable {

	val Sigmoid = new DerivFunc(
			{x => 1 / (1 + exp(-x))},
			{x => {
				val result =  1 / (1 + exp(-x))
				result * (1 - result)}
			})

	val Tanh = new DerivFunc(
			{x => tanh(x)},
			{x => {
				val result = tanh(x)
				1 - result * result}
			})

	val LeakyRelu = new DerivFunc(
			{x => if (x > 0) x else x * 0.1 },
			{x => if (x > 0) 1 else 1 * 0.1 }
		)

}
 

