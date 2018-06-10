package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

@RunWith(classOf[JUnitRunner]) 
class LineOfSightSuite extends FunSuite {
  import LineOfSight._
  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }
  test("parlineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    parLineOfSight(Array[Float](0f, 1f, 8f, 9f), output, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("upsweep") {
    val length = 5
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)

    upsweep(input, 0, input.length, 1)
  }

  test("parlineOfSight big array") {
    val length = 1000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)

    parLineOfSight(input, output, 100)
  }


  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res == 4f)
  }


  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }
  private val input: Array[Float] = Array[Float](0f, 7f, 10f, 33f, 48f)
  test("downsweepSquential should correctly handle a 5 element array when the starting angle is zero") {
    val output = new Array[Float](5)
    downsweepSequential(input, output, 0f, 1, 5)
    assert(output.toList == List(0f, 7f, 7f, 11f,12f))
  }

  test("downsweep should correctly handle a 5 element array when the starting angle is zero") {
    val output = new Array[Float](5)
    val upsweep1: Tree = upsweep(input, 0, 5, 1)
    println(upsweep1)
    downsweep(input, output, 0f, upsweep1)
    assert(output.toList == List(0f, 7f, 7f, 11f,12f))
  }

  test("downsweep") {
    val input = Array[Float](0.0f, 7.0f, 4.0f, 11.0f, 12.0f)
    val expected = Array[Float](0.0f, 0.0f, 0.0f, 0.0f, 0.0f)
    val zeros2 = Array[Float](0.0f, 0.0f, 0.0f, 0.0f, 0.0f)
    lineOfSight(input, expected)

    val tree = upsweep(input, 0, input.length, 1)

    downsweep(input, zeros2, tree.maxPrevious, tree)
    assert(expected === zeros2)
  }

  test("parLineOfSite"){
    val output: Array[Float] = new Array[Float](5)
    parLineOfSight(input, output, 2)
    assert(output.toList == List(0f, 7f, 7f, 11f,12f))
  }

}

