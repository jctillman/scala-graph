package james.linalg.test

import scala.collection.mutable.ListBuffer

import org.scalatest.FunSuite

import james.graph.{MtxFiller => mf}

class MtxFillerTest extends FunSuite {

	test("Can produce variables in a standard deviation"){
		val num = 1000
		val res = (0 to num).map( x => {
			mf.normal(1)(0,0)
		}).toList
		val oneDeviation = res.filter( x => x > -1 && x < 1).length
		val twoDeviation = res.filter( x => x > -2 && x < 2).length
		assert(oneDeviation > num * 0.65 && oneDeviation < num * 0.71)
		assert(twoDeviation > num * 0.92 && oneDeviation < num * 0.99) 
	}

}