package james.linalg.test

import scala.collection.mutable.Map
import scala.util.{Random => r}
import org.scalatest.FunSuite

import scala.collection.mutable.ListBuffer


// import james.graph.GraphSpace
import james.linalg.Mtx
import james.graph._
import james.graph.{MtxFiller => mf}
import james.graph.funcs.{Differentiable => funcs}


class FurtherGraphSpace extends FunSuite {

	//Tests whether we can convert input in accord with the following function.
	//(0,0) -> (1,1)
	//(1,1) -> (0,0)
	//(1,0) -> (0,1)
	//(0,1) -> (1,0)
	test("Testing whether joining sides works.  Pretty sure that it does."){
		val a = new InputMtx("a", (Option(1), Option(2)))
		val aa = new InputMtx("aa", (Option(1), Option(2)))
		val joined = new JoinSide("joined", a, aa)
		val b = new VariableMtx("b", (Option(4), Option(2)), mf.normal(0.1))
		val c = new VariableMtx("c", (Option(1), Option(2)), mf.normal(0.1))
		val outputTemp = new Add("outputTemp", c, new Mult("e", joined, b))

		val n = new VariableMtx("n", (Option(1), Option(2)), mf.normal(0.1))
		val nope = new JoinSide("nope", n, outputTemp)
		val fin = new VariableMtx("fin", ( Option(4), Option(2) ), mf.normal(0.1) )
		val output = new Mult("output", nope, fin)

		val correct = new InputMtx("correct", (Option(1), Option(2)))
		val loss = new MeanSquaredLoss("loss", output, correct)

		var avLoss = 0.0
		var lastLoss = 0.0
		(1 to 500).foreach( x => {
			val input = Map[String, Mtx]()
			val a = r.nextDouble()
			val b = r.nextDouble()
			val sampleInputOne = new Mtx(List(List(a,r.nextDouble)))
			val sampleInputTwo = new Mtx(List(List(b,r.nextDouble)))
			val sampleOutput = new Mtx(List(List(1-b,1-a)))
			input += ("a" -> sampleInputOne)
			input += ("aa" -> sampleInputTwo)
			input += ("correct" -> sampleOutput)
			val output = loss.run(input)
			avLoss = avLoss + output.mx(0)(0)
			loss.adjust(0.5)
			if(x % 10 == 0){
				lastLoss = avLoss
				//println(avLoss / 10)
				avLoss = 0
			}

		})
		assert(lastLoss < 0.001)
	}

	test("Testing whether it is possible to connect something to something else in the past.  Uh huh."){

		// val input = new InputMtx("a", (Option(1), Option(2)))

		// val r1 = new VariableMtx("R", (Option(2), Option(2)), mf.normal(0.1))
		// val w1 = new VariableMtx("W", (Option(2), Option(2)), mf.normal(0.1))

		// val Rm = new Mult("Rm", input.past(1), r1.present() )
		// val b = new VariableMtx("w", (Option(2), Option(2)), mf.normal(0.1))

	}

}