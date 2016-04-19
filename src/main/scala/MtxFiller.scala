package james.graph

import james.util.misc.{normal => mathNormal}

object MtxFiller {

	def normal(stdDev: Double)(a: Int, b: Int): Double = {
		mathNormal(stdDev)
    }
	
}