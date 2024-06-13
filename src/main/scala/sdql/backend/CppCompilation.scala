package sdql
package backend

import java.nio.file.{Path, Paths}
import scala.sys.process.{Process, ProcessBuilder}

object CppCompilation {
  def compile(sdqlFilePath: Path, cpp: String): String = {
    val noExtension = getNoExtension(sdqlFilePath)
    reflect.io.File(cppPath(noExtension).toString).writeAll(cpp)
    inGeneratedDir(clang_format(noExtension)).!!
    inGeneratedDir(clang(noExtension)).!!
    inGeneratedDir(run(noExtension)).!!
  }
  private def clang_format(noExtension: String) = Seq(
    "clang-format", "-i", s"$noExtension.cpp", "-style", s"{ColumnLimit: $clangColumnLimit}"
  )
  private val clangColumnLimit = 120
  private def clang(noExtension: String) = clangCmd ++ Seq(s"$noExtension.cpp", "-o", s"$noExtension.out")
  private def run(noExtension: String) = Seq(s"./$noExtension.out")
  private def cppPath(noExtension: String) = Paths.get(generatedDir.toString, s"$noExtension.cpp")
  private def getNoExtension(path: Path) = reFilename.findAllIn(path.toString).matchData.next().group(2)
  private val reFilename = "^(.+/)*(.+)\\.(.+)$".r

  def cmake(dirPath: Path, fileNames: Array[String]): Unit = {
    val contents = cmakeContents(fileNames.map(dirPath.resolve).map(getNoExtension))
    val path = Paths.get(generatedDir.toString, cmakeFileName)
    reflect.io.File(path.toString).writeAll(contents)
  }
  private def cmakeContents(noExtensions: Seq[String]): String = {
    val init =  s"""#this auto-generated config is handy for debugging
                   |cmake_minimum_required(VERSION $cmakeVersion)
                   |project(generated)
                   |set(CMAKE_CXX_STANDARD $cppStandard)
                   |set(CMAKE_RUNTIME_OUTPUT_DIRECTORY $${CMAKE_CURRENT_SOURCE_DIR})
                   |""".stripMargin
    noExtensions.map(noExtension => s"add_executable($noExtension.out $noExtension.cpp)").mkString(init, "\n", "")
  }
  private val cmakeVersion = "3.28"
  private val cmakeFileName = "CMakeLists.txt"

  def inGeneratedDir(seq: Seq[String]): ProcessBuilder = Process(seq, generatedDir)
  private val generatedDir = new java.io.File("generated")

  private val cppStandard = 20
  val clangCmd: Seq[String] = Seq("clang++", "-Wno-deprecated-builtins", "--std", s"c++$cppStandard")
}
