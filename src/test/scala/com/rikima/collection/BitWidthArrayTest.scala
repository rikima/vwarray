package com.rikima.collection

import org.junit.Assert.{assertTrue, assertEquals}
import org.junit.Test

import scala.util.Random

/**
 * Created by mrikitoku on 15/07/14.
 */
class BitWidthArrayTest {

  @Test def testSetGet = {
    val rowSize: Int  = 10000 * 10000
    val bitWidth: Int = 11
    val seed: Long    = 20120410

    val cbv: BitWidthArray = new BitWidthArray(bitWidth, rowSize)
    val maxValue = cbv.maxValue

    val rand: Random = new Random(seed)
    var t: Long = System.currentTimeMillis
    for (i <- 0 until rowSize) {
      val d = rand.nextDouble
      val v = (d * maxValue).toLong + 1
      assertTrue(v > 0)
      cbv.set(i, v)
      val vv = cbv.get(i)
      //println(s"$i $v $vv")
      assertEquals(v, cbv(i))
    }
    t = System.currentTimeMillis - t
    System.out.println("set time: " + t + " [ms] for #" + rowSize)

    //var t: Long = System.currentTimeMillis
    t = System.currentTimeMillis()
    for (i <- 0 until rowSize) {
      val v = cbv(i)
      assertTrue(v > 0)
    }
    t = System.currentTimeMillis - t
    System.out.println("get time:" + t + "[ms] for #" + rowSize)
    val m = Runtime.getRuntime.totalMemory - Runtime.getRuntime.freeMemory
    System.out.println("memory: " + m / 1024.0 / 1024.0 + " [MB]")
  }
}