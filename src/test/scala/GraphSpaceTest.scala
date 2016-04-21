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


class VctGraphSpace extends FunSuite {

	test("Can multiply some matrices, and get a result--not really testing much"){
		val a = new InputMtx("a", (Option(10), Option(3)))
		val b = new VariableMtx("b", (Option(3), Option(8)), mf.normal(0.1))
		val c = new VariableMtx("c", (Option(10), Option(8)), mf.normal(0.1))
		val d = new Mult("d", a, b)
		val e = new Add("e", d, c)
		val input = Map[String, Mtx]()
		input += ("a" -> Mtx.filledFuncMtx(10,3, mf.normal(0.1)))
		val ret = e.run(input)
		assert(ret.rowNum == 10)
		assert(ret.colNum == 8)
	}


	//Tests whether we can convert input in accord with the following function.
	//(0,0) -> (1,1)
	//(1,1) -> (0,0)
	//(1,0) -> (0,1)
	//(0,1) -> (1,0)
	test("Uber basic backprop"){
			val a = new InputMtx("a", (Option(1), Option(2)))
			val b = new VariableMtx("b", (Option(2), Option(2)), mf.normal(0.1))
			val c = new VariableMtx("c", (Option(1), Option(2)), mf.normal(0.1))
			val output = new Add("add", c, new Mult("e", a, b))
			//val output = new Mult("e", a, b)
			val correct = new InputMtx("correct", (Option(1), Option(2)))
			val loss = new MeanSquaredLoss("loss", output, correct)

			var avLoss = 0.0
			var lastLoss = 0.0
			(1 to 3000).foreach( x => {
				val input = Map[String, Mtx]()
				val sampleInput = new Mtx(List(List(r.nextInt(2), r.nextInt(2))))
				val id = sampleInput.mx(0)(0) //+ sampleInput.mx(0)(1)
				val ib = sampleInput.mx(0)(1)
				val sampleOutput = new Mtx(List(List(1-ib,1-id)))
				input += ("a" -> sampleInput)
				input += ("correct" -> sampleOutput)
				val output = loss.run(input)
				avLoss = avLoss + output.mx(0)(0)
				loss.adjust(0.3)
				if(x % 10 == 0){
					lastLoss = avLoss
					//println(avLoss / 10)
					avLoss = 0
				}

			})
			assert(lastLoss < 0.001)
		}

		test("Uber basic backprop with more complex network"){
			val a = new InputMtx("a", (None, Option(2)))
			val b = new VariableMtx("b", (Option(2), Option(10)), mf.normal(0.1))
			val c = new VariableMtx("c", (Option(1), Option(10)), mf.normal(0.1))
			val d = new Add("d", c, new Mult("wd", a, b))

			val e = new VariableMtx("e", (Option(10), Option(2)), mf.normal(0.1))
			val f = new VariableMtx("f", (Option(1), Option(2)), mf.normal(0.1))
			val output = new Add("output", f, new Mult("wdd", d, e))

			val correct = new InputMtx("correct", (None, Option(2)))
			val loss = new MeanSquaredLoss("loss", output, correct)

			var avLoss = 0.0
			var lastLoss = 0.0
			(1 to 200).foreach( x => {
				val input = Map[String, Mtx]()

				// var samples = ListBuffer[List[Double]]()
				// var outputs = ListBuffer[List[Double]]()
				// (0 until 4).foreach( x => {
				// 	samples += List(r.nextInt(2), r.nextInt(2))
				//     val id = samples(x)(0) 
				//     val ib = samples(x)(1)
				//     outputs += List(1-ib, 1-id)
				// })
				// println(samples.length)
				// println(samples(0).length)
				// val sampleInput = new Mtx(samples.toList)
				// val sampleOutput = new Mtx(outputs.toList)

				val sampleInput = new Mtx(List(List(r.nextInt(2), r.nextInt(2))))
				val id = sampleInput.mx(0)(0) //+ sampleInput.mx(0)(1)
				val ib = sampleInput.mx(0)(1)
				val sampleOutput = new Mtx(List(List(1-ib,1-id)))

				input += ("a" -> sampleInput)
				input += ("correct" -> sampleOutput)
				val output = loss.run(input)
				avLoss = avLoss + output.mx(0)(0)
				loss.adjust(0.25)
				if(x % 10 == 0){
					lastLoss = avLoss
					//println(avLoss / 10)
					avLoss = 0
				}

			})
			assert(lastLoss < 0.001)
		}

		test("Uber basic backprop with more complex network and sigmoid functions"){
			val a = new InputMtx("a", (Option(1), Option(2)))
			val b = new VariableMtx("b", (Option(2), Option(10)), mf.normal(0.1))
			val c = new VariableMtx("c", (Option(1), Option(10)), mf.normal(0.1))
			val d = new Add("d", c, new Mult("wd", a, b))
			val dd = new Transform("dd", d, funcs.Sigmoid)

			val e = new VariableMtx("e", (Option(10), Option(2)), mf.normal(0.1))
			val f = new VariableMtx("f", (Option(1), Option(2)), mf.normal(0.1))
			val output = new Add("output", f, new Mult("wdd", dd, e))

			val correct = new InputMtx("correct", (Option(1), Option(2)))
			val loss = new MeanSquaredLoss("loss", output, correct)

			var avLoss = 0.0
			var lastLoss = 0.0
			(1 to 1000).foreach( x => {
				val input = Map[String, Mtx]()
				val sampleInput = new Mtx(List(List(r.nextInt(2), r.nextInt(2))))
				val id = sampleInput.mx(0)(0) //+ sampleInput.mx(0)(1)
				val ib = sampleInput.mx(0)(1)
				val sampleOutput = new Mtx(List(List(1-ib,1-id)))
				input += ("a" -> sampleInput)
				input += ("correct" -> sampleOutput)
				val output = loss.run(input)
				avLoss = avLoss + output.mx(0)(0)
				loss.adjust(0.1)
				if(x % 100 == 0){
					lastLoss = avLoss / 100
					//println(avLoss / 100 + " d")
					avLoss = 0
				}

			})
			assert(lastLoss < 0.1)
		}


	//Given a vector of five elements, return (1,0)
	//if two of them are 1, otherwise return (0,1)
	test("Less basic backpropogation -- harder problem"){
		val a = new InputMtx("a", (Option(1), Option(5)))

		val w1 = new VariableMtx("w1", (Option(5), Option(15)), mf.normal(0.1))
		val b1 = new VariableMtx("b1", (Option(1), Option(15)), mf.normal(0.1))
		val z1 = new Add("z1", b1, new Mult("z11", a, w1))
		val a1 = new Transform("a1", z1, funcs.LeakyRelu)

		val w2 = new VariableMtx("w2", (Option(15), Option(15)), mf.normal(0.1))
		val b2 = new VariableMtx("b2", (Option(1), Option(15)), mf.normal(0.1))
		val z2 = new Add("z2", b2, new Mult("z22", a1, w2))
		val a2 = new Transform("a2", z2, funcs.LeakyRelu)

		val w3 = new VariableMtx("w3", (Option(15), Option(2)), mf.normal(0.1))
		val b3 = new VariableMtx("b3", (Option(1), Option(2)), mf.normal(0.1))
		val z3 = new Add("z3", b3, new Mult("z33", a2, w3))
		val output = new Transform("output", z3, funcs.LeakyRelu)

		val correct = new InputMtx("correct", (Option(1), Option(2)))
		val loss = new MeanSquaredLoss("loss", output, correct)


		var avLoss = 0.0
		var lastLoss = 0.0
		(1 to 10000).foreach( x => {
			val input = Map[String, Mtx]()
			val sampleInput = new Mtx(List(List(r.nextInt(2), r.nextInt(2), r.nextInt(2), r.nextInt(2), r.nextInt(2))))
			val num = sampleInput.mx(0).sum
			val sampleOutput = new Mtx(List(List(if (num == 2) 1 else 0 ,if (num == 2) 0 else 1)))

			input += ("a" -> sampleInput)
			input += ("correct" -> sampleOutput)
			val ot = loss.run(input)
			avLoss = avLoss + ot.mx(0)(0)
			loss.adjust(0.2)
			if(x % 100 == 0){
				lastLoss = avLoss / 100
				//println(avLoss / 100)
				avLoss = 0
			}
		})
		assert(lastLoss < 0.02)
	}


}