package james.linalg.test

import org.scalatest.FunSuite

import james.linalg.Vct
import scala.util.{Random => r}

class VctDoubleReturns extends FunSuite {

  test("testing lpNorm"){
    var a = new Vct(List(1))
    var b = new Vct(List(1,1))
    var c = new Vct(List(3,4))
    assert(a.lpNorm(1) == 1)
    assert(b.lpNorm(1) == 2)
    assert(a.lpNorm(2) == 1)
    assert(b.lpNorm(2) == Math.pow(2,0.5))
    assert(c.lpNorm(2) == 5)
  }

  test("testing maxNorm"){
    var a = new Vct(List(1))
    var b = new Vct(List(1,1))
    var c = new Vct(List(3,4))
    assert(a.maxNorm() == 1)
    assert(c.maxNorm() == 4)
  }

  test("testing dot product"){
    var a = new Vct(List(1))
    var b = new Vct(List(1,1))
    var c = new Vct(List(3,4))
    assert(b.dot(c) == 7)
    assert(c.dot(b) == 7)
  }

}

class VctBooleanReturns extends FunSuite {
  
  test("testing isUnit"){
    (1 until 100).foreach( x => {
      var angle = r.nextDouble
      var vc = new Vct(List(Math.cos(angle),Math.sin(angle)))
      assert( (vc.isUnit() == true) )
    })
  }

  test("testing isOrthonormalTo"){
     (1 until 100).foreach( x => {
      var angle = r.nextDouble
      var vc = new Vct(List(Math.cos(angle),Math.sin(angle)))
      var vd = new Vct(List(-Math.sin(angle),Math.cos(angle)))
      assert( (vc.isOrthonormalTo(vd) == true) )
    })
  }

}

class VctVctReturns extends FunSuite {
  test("testing softmax"){
    (1 until 100).foreach( x => {
      var ok = new Vct(List(r.nextDouble, r.nextDouble*3, r.nextDouble*2))
      var maxed = ok.softMax(r.nextDouble*10)
      var total = maxed.col(0).sum
      assert(Math.abs(total - 1) < 0.00001)
    })
  }
}

