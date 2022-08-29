package sdql
package storage

import java.io.FileReader
import java.io.BufferedReader
import java.text.SimpleDateFormat
import ir._

/**
  * Code from: https://github.com/epfldata/dblab/blob/develop/components/
  *             src/main/scala/ch/epfl/data/dblab/storagemanager/
  *
  * An efficient Scanner defined for reading from files.
  *
  */

object Loader {
 
  def fileLineCount(file: String): Int = {
    import java.nio.file.Files
    Files.lines(java.nio.file.Paths.get(file)).count().toInt
  }

 def loadTable(table: Table, limit: Int = -1): Array[RecordValue] = {
      val fileSize = fileLineCount(table.resourceLocator)
      val size = if(limit == -1) fileSize else math.min(fileSize, limit)
      val arr = new Array[RecordValue](size)
      val ldr = new FastScanner(table.resourceLocator)
      var i = 0
      while (i < size && ldr.hasNext()) {
        val values = table.attributes.map(arg =>
          arg.tpe match {
            case IntType => ldr.next_int
            case LongType => ldr.next_long
            case RealType        => ldr.next_double
            case StringType     => ldr.next_string
            case DateType         => DateValue(ldr.next_date)
            case t => raise(s"Not handled type `$t` in the loader.")
          })
        arr(i) = RecordValue(table.attributes.map(_.name).zip(values).toSeq)
        i += 1
      }
      arr
  }

  def loadNodesTable(table: Table, limit: Int = -1): Map[Int,RecordValue] = {
    val fileSize = fileLineCount(table.resourceLocator)
    val size = if(limit == -1) fileSize else math.min(fileSize, limit)
    val arr = new Array[RecordValue](size)
    val ldr = new FastScanner(table.resourceLocator)
    var dict:Map[Int,RecordValue] = Map()
    var i = 0
    while (i < size && ldr.hasNext()) {
      val values = table.attributes.map(arg => {
        arg.tpe match {
          case IntType => ldr.next_int
          case LongType => ldr.next_long
          case RealType => ldr.next_double
          case StringType => ldr.next_string
          case DateType => DateValue(ldr.next_date)
          case t => raise(s"Not handled type `$t` in the loader.")
        }
      })
      arr(i) = RecordValue(table.attributes.map(_.name).zip(values).toSeq)
      dict+=(values(0).asInstanceOf[Int]->arr(i))
      i += 1
    }
    dict
  }

}