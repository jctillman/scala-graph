package james.linalg.test

// import com.sksamuel.scrimage._
// import java.io._
import org.scalatest.FunSuite

import math._

import java.io.File
import java.awt.Color
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import james.graph.funcs.{Differentiable => funcs}
import james.graph.{MtxFiller => mf}
import scala.util.{Random => r}
import scala.collection.mutable.Map

import james.linalg.Mtx
import james.graph._

class ImageDrawing extends FunSuite {

	test("Loading image"){

		val f = new File("/Users/jamestillman/Documents/prog/scala/agi_exp/src/test/scala/toBePainted.jpg")
		val img = ImageIO.read(f)
		
		val ww = img.getWidth
  		val hh = img.getHeight

  		val dimensions = 2

  		val out = new BufferedImage(ww*dimensions, hh*dimensions, BufferedImage.TYPE_INT_RGB)

  		val a = new InputMtx("a", (Option(1), Option(2)))

		val w1 = new VariableMtx("w1", (Option(2), Option(70)), mf.normal(0.1))
		val b1 = new VariableMtx("b1", (Option(1), Option(70)), mf.normal(0.1))
		val z1 = new Add("z1", b1, new Mult("z11", a, w1))
		val a1 = new Transform("a1", z1, funcs.LeakyRelu)

		val w2 = new VariableMtx("w2", (Option(70), Option(70)), mf.normal(0.1))
		val b2 = new VariableMtx("b2", (Option(1), Option(70)), mf.normal(0.1))
		val z2 = new Add("z2", b2, new Mult("z22", a1, w2))
		val a2 = new Transform("a2", z2, funcs.LeakyRelu)

		val w3 = new VariableMtx("w3", (Option(70), Option(70)), mf.normal(0.1))
		val b3 = new VariableMtx("b3", (Option(1), Option(70)), mf.normal(0.1))
		val z3 = new Add("z3", b3, new Mult("z33", a2, w3))
		val a3 = new Transform("a3", z3, funcs.LeakyRelu)

		val w33 = new VariableMtx("w3", (Option(70), Option(70)), mf.normal(0.1))
		val b33 = new VariableMtx("b3", (Option(1), Option(70)), mf.normal(0.1))
		val z33 = new Add("z3", b33, new Mult("z33", a3, w33))
		val a33 = new Transform("a3", z33, funcs.LeakyRelu)

		val w4 = new VariableMtx("w4", (Option(70), Option(70)), mf.normal(0.1))
		val b4 = new VariableMtx("b4", (Option(1), Option(70)), mf.normal(0.1))
		val z4 = new Add("z4", b2, new Mult("z44", a33, w4))
		val a4 = new Transform("a4", z4, funcs.LeakyRelu)

		val w5 = new VariableMtx("w5", (Option(70), Option(1)), mf.normal(0.1))
		val b5 = new VariableMtx("b5", (Option(1), Option(1)), mf.normal(0.1))
		val z5 = new Add("z5", b5, new Mult("z55", a4, w5))
		val output = new Transform("output", z5, funcs.LeakyRelu)

		val correct = new InputMtx("correct", (Option(1), Option(2)))
		val loss = new MeanSquaredLoss("loss", output, correct)


		var avLoss = 0.0
		var lastLoss = 0.0
		(1 to 4000000).foreach( x => {

			
			val input = Map[String, Mtx]()

			val xVal = r.nextDouble
			val yVal = r.nextDouble
			val sampleX: Int = floor(xVal * ww).toInt
			val sampleY: Int = floor(yVal * hh).toInt


			val n = img.getRGB(sampleX, sampleY)
			val outPut = new Color(n).getRed / 255.00
			//println(xVal, yVal, outPut)

			val sampleInput = new Mtx(List(List(xVal, yVal)))
			val sampleOutput = new Mtx(List(List(outPut)))

			input += ("a" -> sampleInput)
			input += ("correct" -> sampleOutput)
			val ot = loss.run(input)
			avLoss = avLoss + ot.mx(0)(0)
			loss.adjust(0.005)
			if(x % 100 == 0){
				lastLoss = avLoss / 100
				println(x + " of 4,000,000: " + avLoss / 100)
				avLoss = 0
			}

		})


  		  // copy pixels (mirror horizontally)
		for (x <- 0 until ww * dimensions){
			for (y <- 0 until hh * dimensions){

				val sampleX = (x * 1.0 / ww) / dimensions
				val sampleY = (y * 1.0 / hh) / dimensions
				//println(sampleY, sampleX)
				val sampleInput = new Mtx(List(List(sampleX, sampleY)))
				val input = Map[String, Mtx]()
				input += ("a" -> sampleInput)
				val n = (output.run(input).mx(0)(0) * 255).toInt
				//println(n)

				val c = if(n > 0) new Color(n,n,n) else new Color(0,0,0)
				out.setRGB(x, y, c.getRGB)
			}
			println(x)
		}

		ImageIO.write(out, "png", new File("/Users/jamestillman/Documents/prog/scala/agi_exp/src/test/scala/test.png"))
	}
  
}


