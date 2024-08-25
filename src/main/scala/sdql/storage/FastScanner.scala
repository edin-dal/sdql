package sdql
package storage

import java.io.{ BufferedReader, FileReader }

/**
 * Code from: https://github.com/epfldata/dblab/blob/develop/components/
 *             src/main/scala/ch/epfl/data/dblab/storagemanager/
 *
 * An efficient Scanner defined for reading from files.
 *
 */
class FastScanner(filename: String) {

  private var byteRead: Int      = 0
  private var intDigits: Int     = 0
  private var delimiter: Char    = '|'
  private val br: BufferedReader = new BufferedReader(new FileReader(filename))

  def next_int(): Int = {
    var number = 0
    var signed = false

    intDigits = 0
    byteRead = br.read()
    if (byteRead == '-') {
      signed = true
      byteRead = br.read()
    }
    while (Character.isDigit(byteRead)) {
      number *= 10
      number += byteRead - '0'
      byteRead = br.read()
      intDigits = intDigits + 1
    }
    if ((byteRead != delimiter) && (byteRead != '.') && (byteRead != '\n'))
      throw new RuntimeException(
        "Tried to read Integer, but found neither delimiter nor . after number (found " +
          byteRead.asInstanceOf[Char] + ", previous token = " + intDigits + "/" + number + ")"
      )
    if (signed) -1 * number else number
  }

  def next_double(): Double = {
    val numeral = next_int().toDouble
    var fractal = 0.0
    // Has fractal part
    if (byteRead == '.') {
      fractal = next_int().toDouble
      while (intDigits > 0) {
        fractal = fractal * 0.1
        intDigits = intDigits - 1
      }
    }
    if (numeral >= 0) numeral + fractal
    else numeral - fractal
  }

  def next_char(): Char = {
    byteRead = br.read()
    val del = br.read() //delimiter
    if ((del != delimiter) && (del != '\n'))
      throw new RuntimeException("Expected delimiter after char. Not found. Sorry!")
    byteRead.asInstanceOf[Char]
  }

  def next(buf: Array[Byte]): Int =
    next(buf, 0)

  def next(buf: Array[Byte], offset: Int): Int = {
    byteRead = br.read()
    var cnt = offset
    while (br.ready() && (byteRead != delimiter) && (byteRead != '\n')) {
      buf(cnt) = byteRead.asInstanceOf[Byte]
      byteRead = br.read()
      cnt += 1
    }
    cnt
  }

  private val buffer = new Array[Byte](1 << 10)
  def next_string: String = {
    java.util.Arrays.fill(buffer, 0.toByte)
    byteRead = br.read()
    var cnt = 0
    while (br.ready() && (byteRead != delimiter) && (byteRead != '\n')) {
      buffer(cnt) = byteRead.asInstanceOf[Byte]
      byteRead = br.read()
      cnt += 1
    }
    val resultArray = new Array[Byte](cnt)
    System.arraycopy(buffer, 0, resultArray, 0, cnt)
    new String(resultArray.map(_.toChar))
  }

  def next_date: Int = {
    delimiter = '-'
    val year  = next_int()
    val month = next_int()
    delimiter = '|'
    val day = next_int()
    //val date_str = year + "-" + month + "-" + day
    year * 10000 + month * 100 + day
  }

  def hasNext(): Boolean = {
    val f = br.ready()
    if (!f) br.close
    f
  }

  def close(): Unit =
    br.close()
}
