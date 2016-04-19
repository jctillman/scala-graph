
package james.linalg

import scala.math._
import james.util.misc.{transpose => listTranspose}
import james.util.deeps.{map => deepMap}
import james.util.deeps.{combine => deepCombine}
import james.util.deeps.{equal => deepEqual}
import james.util.deeps.{compare => permuteCompare}

class Mtx(data: List[List[Double]]) {

	//Only data in the whole thing.
	val mx: List[List[Double]] = data
	val rowNum = mx.length;
	val colNum = mx(0).length;
	val rowRange = 0 until rowNum
	val colRange = 0 until colNum
	override def toString: String = { mx.toString}

	def rows(): List[List[Double]] = mx
	def cols(): List[List[Double]] = {listTranspose[Double](mx)}
	def row(index: Integer): List[Double] = {rows()(index)}
	def col(index: Integer): List[Double] = {cols()(index)}

	//Return double
	def trace(): Double = {
		0.until(min(rowNum, colNum)).map(x => mx(x)(x)).sum
	}
	def frobenius(): Double = {
		sqrt(mx.flatten.map( x=> x*x).sum)
	}
	def determinant(): Double = {
		assert(this.isSquare())
		if (rowNum == 1){
			this.mx(0)(0)
		}else{
			val self = this
			this.row(0).zipWithIndex.foldLeft(0.0)( (prior: Double, n: (Double, Int)) => {
				val top = n._1
				val index = n._2
				prior + (if (index % 2 == 0) 1 else -1) * top * self.minor(0,index).determinant()
			})
		}
	}
	def det(): Double = this.determinant();


	//Return Boolean
	def isSquare(): Boolean = {rowNum == colNum}
	def canEqual(a: Any): Boolean = a.isInstanceOf[Mtx]
	override def equals(other: Any): Boolean = {
		other match {
			case other: Mtx => {
				deepEqual[Double](mx, other.mx, (x: Double, y: Double) => Math.abs(x - y) < 0.00001)}
			case _ => false
		}
	}
	def isSymmetric(): Boolean = {
		this.transpose().mx == this.mx
	}
	def isOrthogonal(): Boolean = {
		def func(x: List[Double], y: List[Double]): Boolean = new Vct(x).isOrthonormalTo(new Vct(y))
		this.isSquare() &&
			permuteCompare(this.cols(), func) &&
			permuteCompare(this.rows(), func)
	}


	//Return Mtx
	def transpose(): Mtx = { new Mtx(this.cols())}
	def trans(): Mtx = this.transpose()

	def add(other: Mtx): Mtx = { new Mtx(deepCombine[Double](mx, other.mx, _+_))}
	def +(other: Mtx): Mtx = this.add(other)
	def sub(other: Mtx): Mtx = this.add(other.neg())
	def -(other: Mtx): Mtx = this.sub(other)
	def hada(other: Mtx): Mtx = { new Mtx(deepCombine[Double](mx, other.mx, _*_))}
	def hadamard(other: Mtx): Mtx = this.hada(other)
	def piecewise(func: Double => Double) = {new Mtx(deepMap[Double](mx, func))}
	def scalarMult(other: Double): Mtx = piecewise(_ * other)
	def neg(): Mtx = scalarMult(-1)
	def mult(other: Mtx): Mtx = {
		assert(colNum == other.rowNum)
		val newOne = rowRange.map( a => 
				other.colRange.map( b => {
					val fst = mx(a)
					val snd = other.mx.map( x => x(b))
					fst.zip(snd).map({case (x: Double, y: Double) => x * y }).sum
				}).toList
			).toList
		new Mtx(newOne)
	}
	def *(other: Mtx) = this.mult(other)
	def minor(row: Integer, col: Integer): Mtx = {
		val temp = mx.zipWithIndex.
			filter({case (x: List[Double], index: Int) => index != row}).
			map(x => x._1).
			map( (x: List[Double]) => x.zipWithIndex.filter({ case (y: Double, z: Int) => z != col}).map(y => y._1))
		new Mtx(temp)
	}
	def inverse(): Mtx = {
		val determinant = this.det();
		assert(this.isSquare())
		assert(determinant != 0)
		if (rowNum == 1){
			new Mtx(List(List( 1 / mx(0)(0) )))
		}else if (rowNum == 2){
			val a = mx(0)(0)
			val b = mx(0)(1)
			val c = mx(1)(0)
			val d = mx(1)(1)
			val temp = new Mtx(List(List(d,-b),List(-c,a)))
			val tempDet = temp.det();
			temp.scalarMult(1/tempDet);
		}else{
			val sub = Mtx.splitSquare(mx)
			val a = new Mtx(sub(0))
			val b = new Mtx(sub(1))
			val c = new Mtx(sub(2))
			val d = new Mtx(sub(3))
			val aInverse = a.inverse()
			val temp = (d - (c.mult(aInverse).mult(b))).inverse()
			val a1 = aInverse + (aInverse * b * temp * c * aInverse)
			val b1 = aInverse.neg() * b * temp
			val c1 = temp.neg() * c * aInverse
			val d1 = temp
			new Mtx( Mtx.joinList(a1.mx, b1.mx, c1.mx, d1.mx) )
		}
	}
	def rowSwap(a: Int, b: Int): Mtx = {
		val min = Math.min(a,b)
		val maj = Math.max(a,b)
		new Mtx(mx.take(min) ++ 
			List(mx(maj)) ++ 
			mx.drop(min+1).take(maj-min-1) ++ 
			List(mx(min)) ++ 
			mx.drop(maj+1))
	}
	def rowMult(index: Int, value: Double): Mtx = {
		new Mtx(mx.take(index) ++
				List(mx(index).map(_*value)) ++
				mx.drop(index+1))
	}
	def rowAdd(from: Int, to: Int): Mtx = {
		new Mtx(mx.take(to) ++
				List( mx(to).zip(mx(from)).map({
					case (x: Double, y:Double)=> x+y}) ) ++ 
				mx.drop(to+1))
	}
	def gaussJordan(aug: Boolean): Mtx = {
		val augBonus = if (aug) 1 else 0
		def GJInner(a: Mtx, i: Int, j: Int): Mtx = {
			if 		(i >= a.rowNum) a
			else if (j >= a.colNum - augBonus) a
			else {
				val thisAllZero = a.row(i).forall(Math.abs(_) < 0.0000000001)
				val nextNonZeroCol = a.col(j).zipWithIndex.
					foldLeft(i)({case (old: Int, (x: Double, dex: Int)) =>
							if (Math.abs(x) > 0.0000000001 && dex > i) dex else old
						})
				
				if (Math.abs(a.mx(i)(j)) < 0.0000001 && nextNonZeroCol != i){
					GJInner(a.rowSwap(i, nextNonZeroCol), i,j)
				}else if (thisAllZero){
					GJInner(a, i+1, j)
				}else if (a.mx(i)(j) == 0){
					GJInner(a, i, j+1)
				}else{
					var b = a.rowMult(i, 1/a.mx(i)(j))
					(0 until b.rowNum).foreach( x => {
						if (b.mx(x)(j) != 0 && x != i){
							val n = -b.mx(x)(j)
							b = b.rowMult(i, -b.mx(x)(j))
							b = b.rowAdd(i, x)
							b = b.rowMult(i, 1/n)
						}
					})
					GJInner(b, i+1,j+1)
				}
			}
		}
		GJInner(new Mtx(mx), 0,0)
	}
}



object Mtx {

	def splitSquare(x: List[List[Double]]): List[List[List[Double]]] = {
		val rows = x.length
		val cols = x(0).length
		val middle = Math.round(rows / 2)
		val a: List[List[Double]] = cut(x, 0, middle, 0, middle)
		val b: List[List[Double]] = cut(x, 0, middle, middle, cols)
		val c: List[List[Double]] = cut(x, middle, rows, 0, middle)
		val d: List[List[Double]] = cut(x, middle, rows, middle, cols)
		List(a,b,c,d)
	}

	def cut(b: List[List[Double]], rowStart: Int, rowEnd: Int, colStart: Int, colEnd: Int): List[List[Double]] = {
		val rows = rowStart until rowEnd
		val cols = colStart until colEnd
		rows.map( row => cols.map( col => b(row)(col) ).toList).toList
	}

	def joinList(a: List[List[Double]], b: List[List[Double]], c: List[List[Double]], d: List[List[Double]]): List[List[Double]] = {
		val height = a.length + c.length
		val width = a(0).length + b(0).length
		(0 until height).map( row => 
			(0 until width).map ( col => 
					if (row < a.length){
							if(col < a(0).length){
								a(row)(col)
							}else{
								b(row)(col - a(0).length)
							}
						}else{
							if (col < a(0).length){
								c(row - a.length)(col)
							}else{
								d(row - a.length)(col - a(0).length)
							}
						}
				).toList).toList

	}

	def filledMtx(sizeRow: Integer, sizeCol: Integer, contents: Double): Mtx = {
		new Mtx(List.tabulate(sizeRow,sizeCol)( (x,y) => contents))
	}

	def filledFuncMtx(sizeRow: Integer, sizeCol: Integer, contents: (Int,Int) => Double): Mtx = {
		new Mtx(List.tabulate(sizeRow,sizeCol)(contents))
	}

	//This has to be made neater somehow.
	def diagMtx(c: List[Double]): Mtx = {
		new Mtx(List.tabulate(c.length,c.length)( (x,y) => if (x == y) c(x) else 0))
	}

	def identMtx(size: Integer): Mtx = {
		diagMtx(List.fill(size)(1))
	}

}

