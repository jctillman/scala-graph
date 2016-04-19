package james.linalg.test

import org.scalatest.FunSuite

import scala.util.{Random => r}
import james.linalg.Mtx


class MtxBooleanReturns extends FunSuite {

  test("testing isSquare"){
    val n = new Mtx(List(List(1,2),List(2,1)))
    val m = new Mtx(List(List(1,2),List(1,2)))
    val o = new Mtx(List(List(1,2,3),List(2,1,3)))
    val p = new Mtx(List(List(1,2),List(2,1)))
    assert(n.isSquare() == true)
    assert(m.isSquare() == true)
    assert(o.isSquare() == false)
  }

  test("testing isSymmetric"){
    val n = new Mtx(List(List(1,2),List(2,1)))
    val m = new Mtx(List(List(1,2),List(1,2)))
    val o = new Mtx(List(List(1,2,3),List(2,1,3)))
    val p = new Mtx(List(List(1,2),List(2,1)))
  	assert(n.isSymmetric() == true)
  	assert(m.isSymmetric() == false)
  	assert(o.isSymmetric() == false)
  }

  test("testing equals"){
    val n = new Mtx(List(List(1,2),List(2,1)))
    val m = new Mtx(List(List(1,2),List(1,2)))
    val o = new Mtx(List(List(1,2,3),List(2,1,3)))
    val p = new Mtx(List(List(1,2),List(2,1)))
    assert(n.equals(p))
    assert(n != m)
    assert(n != o)
    assert(n == p)
    println("equals works")
  }

  test("testing isOrthogonal"){
    val a = new Mtx(List(List(1,0),List(0,1)))
    val b = new Mtx(List(List(1,0),List(0,-1)))
    val c = new Mtx(List(List(0,-.8,-.6),List(.8,-.36,.48),List(.6,.48,-.64)))
    val d = new Mtx(List(List(0,0,1),List(0,1,0),List(1,0,0)))
    val e = new Mtx(List(List(0,-.9,-.6),List(.8,-.36,.48),List(.6,.48,-.64)))
    assert(a.isOrthogonal())
    assert(b.isOrthogonal())
    assert(c.isOrthogonal())
    assert(d.isOrthogonal())
    assert(!e.isOrthogonal())
  }

}

class MtxMtxReturns extends FunSuite {

  test("testing add"){
    val l = new Mtx(List(List(0,0), List(0,0)))
  	val n = new Mtx(List(List(1,0), List(0,1)))
  	val m = new Mtx(List(List(2,1), List(1,2)))
  	val o = new Mtx(List(List(1,1), List(1,1)))
  	val p = new Mtx(List(List(2,2), List(2,2)))
  	val q = new Mtx(List(List(4,2), List(2,4)))
  	assert(n + o == m)
  	assert(o + n == m)
  	assert(l + l == l)
  	assert(l + m == m)
  }

  test("testing subtract"){
    val l = new Mtx(List(List(0,0), List(0,0)))
    val n = new Mtx(List(List(1,0), List(0,1)))
    val m = new Mtx(List(List(2,1), List(1,2)))
    val o = new Mtx(List(List(1,1), List(1,1)))
    val p = new Mtx(List(List(2,2), List(2,2)))
    val q = new Mtx(List(List(4,2), List(2,4)))
  	assert(m - n == o)
  	assert(m - l == m)
  	assert(o - l == o)
  }

  test("testing hadamard"){
    val l = new Mtx(List(List(0,0), List(0,0)))
    val n = new Mtx(List(List(1,0), List(0,1)))
    val m = new Mtx(List(List(2,1), List(1,2)))
    val o = new Mtx(List(List(1,1), List(1,1)))
    val p = new Mtx(List(List(2,2), List(2,2)))
    val q = new Mtx(List(List(4,2), List(2,4)))
    assert(o.hada(o) == o)
    assert(n.hada(o) == n)
    assert(m.hada(p) == q)
  }

  test("testing scalarMult"){
    val l = new Mtx(List(List(0,0), List(0,0)))
    val n = new Mtx(List(List(1,0), List(0,1)))
    val m = new Mtx(List(List(2,1), List(1,2)))
    val o = new Mtx(List(List(1,1), List(1,1)))
    val p = new Mtx(List(List(2,2), List(2,2)))
    val q = new Mtx(List(List(4,2), List(2,4)))
    assert(o.scalarMult(2.0) == p)
    assert(m.scalarMult(2.0) == q)
  }

  test("testing mult"){
  	val z = new Mtx(List(List(1,2,3),List(4,5,6)))
  	val y = new Mtx(List(List(7,8), List(9,10), List(11,12)))
  	val x = new Mtx(List(List(58, 64), List(139, 154)))
  	val ident2 = Mtx.identMtx(2)
  	assert(z * y == x)
  	assert(x * ident2 == x)
    assert(ident2 * x == x)
  }

  test("testing minor"){
    val a = new Mtx(List(List(1,2),List(3,4)))
    val b = new Mtx(List(List(1)))
    val c = new Mtx(List(List(4)))
    val d = new Mtx(List(List(1,1,1),List(1,2,2),List(3,4,4)))
    assert(a.minor(0,0) == c)
    assert(a.minor(1,1) == b)
    assert(d.minor(0,2) == a)
  }


  test("testing matrix inversion"){
    (2 until 8).foreach( rank => {
      (1 until 20).foreach( num => {
        val inner = (0 until rank).map(
          x => (0 until rank).map(
            y => r.nextDouble).toList).toList
        val temp = new Mtx(inner)
        val inv = temp.inverse()
        val ident2 = Mtx.identMtx(rank)
        assert(temp * inv == ident2)
        assert(inv * temp == ident2)
      })
      println("inversion works for matrices of rank" + rank)
    })
  }

  test("testing rowSwap"){
    val aa = new Mtx(List(List(1,2),List(3,4)))
    val bb = new Mtx(List(List(3,4),List(1,2)))
    assert(aa.rowSwap(0,1) == bb)
    assert(aa.rowSwap(1,0) == bb)

    val aaa = new Mtx(List(List(1,2,3),List(3,4,5),List(5,6,7)))
    val bbb = new Mtx(List(List(3,4,5),List(1,2,3),List(5,6,7)))
    val ccc = new Mtx(List(List(5,6,7),List(3,4,5),List(1,2,3)))
    assert(aaa.rowSwap(1,0) == bbb)
    assert(aaa.rowSwap(0,1) == bbb)
    assert(aaa.rowSwap(2,0) == ccc)
    assert(aaa.rowSwap(0,2) == ccc)

    val aaaa = new Mtx(
      List(List(1,2,3,4),List(1,2,3,4),List(5,6,7,8),List(5,6,7,8)))
    val bbbb = new Mtx(
      List(List(1,2,3,4),List(5,6,7,8),List(5,6,7,8),List(1,2,3,4)))
    assert(aaaa.rowSwap(0,1) == aaaa)
    assert(aaaa.rowSwap(1,0) == aaaa)
    assert(aaaa.rowSwap(1,3) == bbbb)
    assert(aaaa.rowSwap(0,1).rowSwap(3,1) == bbbb)
    assert(aaaa.rowSwap(1,0).rowSwap(1,3).rowSwap(0,3) == bbbb)
  }

  test("testing rowMult"){

    val ab = new Mtx(List(List(1,1),List(1,1)))
    val ac = new Mtx(List(List(2,2),List(4,4)))
    val ad = new Mtx(List(List(1,1),List(2,2)))
    assert(ab.rowMult(1,2) == ad)
    assert(ab.rowMult(0,2).rowMult(1,4) == ac)
  }

  test("testing rowAdd"){
    val bh = new Mtx(List(List(1,2),List(3,4)))
    val bd = new Mtx(List(List(1,2),List(4,6)))
    val be = new Mtx(List(List(4,6),List(3,4)))
    val bf = new Mtx(List(
      List(1,2,3,4),List(5,6,7,8),List(0,0,0,0),List(0,0,0,0)))
    val bg = new Mtx(List(
      List(1,2,3,4),List(5,6,7,8),List(1,2,3,4),List(0,0,0,0)))
    assert(bh.rowAdd(0,1) == bd)
    assert(bh.rowAdd(1,0) == be)
    assert(bf.rowAdd(0,2) == bg)
  }


  test("testing gaussJordan"){

    (2 until 20).foreach( rank => {
      (1 until 20).foreach( num => {
        val inner = (0 until rank).map(
          x => (0 until rank).map(
            y => r.nextDouble).toList).toList
        val temp = new Mtx(inner)
        val jordaned = temp.gaussJordan(false)
        val ident2 = Mtx.identMtx(rank)
        assert(jordaned == ident2)
      })
      println("gauss Jordan reduces matrixes of dimension " + rank)
    })

  }

}


    
class MtxDoubleReturns extends FunSuite {

  test("Double-returning functions return accurate values"){
    val a = new Mtx(List(List(-2,2,-3),List(-1,1,3),List(2,0,-1)))
    assert(a.det() == 18)
    println("determinant works")

    assert(a.trace() == -2)
    println("trace works")

    assert(a.frobenius() == Math.sqrt(33))
    println("frobenius works")

  }

}

class MtxStatic extends FunSuite {
  test("Static functions return accurate values"){

    println("\n\nTesting helper matrix returning functions..")

    val a = new Mtx(List(List(10,10),List(10,10)))
    val b = new Mtx(List(List(10,0),List(0,10)))
    val c = new Mtx(List(List(1,0),List(0,1)))

    val aa = Mtx.filledMtx(2,2,10)
    assert(aa == a)
    println("filledMtx works")

    val bb = Mtx.diagMtx(List(10,10))
    assert(bb == b)
    println("diagMtx works")

    val cc = Mtx.identMtx(2)
    assert(cc == c)
    println("identMtx works")
  }

}
