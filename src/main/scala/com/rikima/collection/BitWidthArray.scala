package com.rikima.collection;

/**
 * Created by mrikitoku on 15/07/14.
 */
class BitWidthArray(bitWidth: Int, size: Int) extends Seq[Long] {
  var debug: Boolean        = false

  // fields ---------------
  val LONG_SIZE: Int        = 64
  val innerArray            = new Array[Long]( (size * bitWidth) / LONG_SIZE + 1 )
  val maxValue              = (1 << bitWidth) - 1
  val LongSizeMinusBitWidth = LONG_SIZE - this.bitWidth
  val mask: Long            = this.maskValue(bitWidth)

  // methods --------------
  override def length: Int = this.size

  override def apply(idx: Int): Long = this.get(idx)

  override def iterator: Iterator[Long] = {
    null
  }

  def getBitWidth: Int = this.bitWidth

  def get (row: Int): Long = {
    val arrayIndex = row * this.bitWidth / LONG_SIZE
    val offset     = row * this.bitWidth % LONG_SIZE

    if ((offset + this.bitWidth) <= LONG_SIZE) {
      val v = (this.innerArray(arrayIndex) << offset) >>> this.LongSizeMinusBitWidth
      v
    } else {
      val offset2 = (row + 1) * this.bitWidth % LONG_SIZE
      val uv = (this.innerArray(arrayIndex) << offset) >>> this.LongSizeMinusBitWidth
      val lv = this.innerArray(arrayIndex + 1) >>> (LONG_SIZE - offset2)
      uv + lv
    }
  }

  def set (row: Int, value: Long) {
    //assert (this.rangeCheck (value) )
    val arrayIndex = row * this.bitWidth / LONG_SIZE
    val offset     = row * this.bitWidth % LONG_SIZE

    if (offset + this.bitWidth <= LONG_SIZE) {
      val shift = this.LongSizeMinusBitWidth - offset
      this.innerArray (arrayIndex) = (this.innerArray (arrayIndex) | value << shift)
    } else {
      val shift = offset - this.LongSizeMinusBitWidth
      this.innerArray (arrayIndex) = (this.innerArray (arrayIndex) | value >>> shift)

      var mask = 1L
      for (j <- 0 until shift) {
        mask = mask | mask << 1
      }

      val v = value & mask
      this.innerArray (arrayIndex + 1) = v << (LONG_SIZE - shift)
    }
  }

  def usedByte: Int = this.innerArray.length * LONG_SIZE / 8 // Byte.SIZE

  def getRowSize: Long = this.size

  def innerArraySize: Int = this.innerArray.length

  def getMask: Long = {
    assert (this.mask > 0)
    this.mask
  }

  def maskValue (bitWidth: Int): Long = {
    var m = 1L
    for (j <- 1 until bitWidth) {
      m = m | m << 1
    }
    m
  }

  def rangeCheck (value: Long): Boolean = value > 0 && value <= this.maxValue
  /*
  def check: Boolean = { {
    var i: Int = 0
    while (i < this.rowSize) { {
      val `val`: Long = this.get (i)
      if (`val` <= 0) {
        false
      }
    }
      ( {
        i += 1;
        i
      })
    }
  }
    true
  }
  */
}
