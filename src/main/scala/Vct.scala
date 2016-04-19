package james.linalg;

class Vct(lst: List[Double]) extends Mtx(lst.map( x=> List(x) )) {

	//Those returning scalar values
	def lpNormPowered(p: Int): Double = this.col(0).foldLeft(0.0)( (x,y) => x + Math.pow(y, p))
	def lpNorm(p: Int): Double = Math.pow(this.lpNormPowered(p), 1.0/p)
	def maxNorm(): Double = this.col(0).reduceLeft(_ max _)
	def euclideanNorm(): Double = lpNorm(2)
	def dot(other: Vct): Double = {
		other.col(0).zip(this.col(0)).foldLeft(0.0)({ case (old, (x,y)) => old + x*y }) 
	}

	//those returning boolean
	def isOrthogonalTo(other: Vct): Boolean = Math.abs(this.dot(other)) < 0.0001
	def isUnit(): Boolean = Math.abs(this.euclideanNorm() - 1) < 0.00001
	def isOrthonormalTo(other: Vct): Boolean = {
		isOrthogonalTo(other) && this.isUnit() && other.isUnit()
	}

	//Returns the vector softmaxized
	def softMax(temp: Double): Vct = {
		var sum = this.col(0).map(Math.exp(_)).sum
		new Vct ( this.col(0).map(Math.exp(_)/sum) )
	}
 
}
