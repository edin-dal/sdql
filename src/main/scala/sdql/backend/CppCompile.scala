package sdql
package backend
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Path, Paths }
import scala.sys.process.{ Process, ProcessBuilder }

object CppCompile {
  def compile(sdqlFilePath: String, cpp: String): String = {
    writeFormat(sdqlFilePath, cpp)
    compileRun(sdqlFilePath)
  }

  def writeFormat(sdqlFilePath: String, cpp: String): Unit = {
    val noExtension = getNoExtension(sdqlFilePath)
    write(cppPath(noExtension), cpp)
    val _ = inGeneratedDir(clangFormat(noExtension)).!!
  }

  private def compileRun(sdqlFilePath: String): String = {
    val noExtension = getNoExtension(sdqlFilePath)
    inGeneratedDir(clang(noExtension)).!!
    prettyPrint(inGeneratedDir(run(noExtension)).!!)
  }

  private def prettyPrint(s: String): String =
    s.replace("\u0000", "").split("\n").sorted.mkString("\n")

  private def write(path: Path, contents: String) = Files.write(path, contents.getBytes(StandardCharsets.UTF_8))

  private def clangFormat(noExtension: String) = Seq(
    "clang-format",
    "-i",
    s"$noExtension.cpp",
    "-style",
    s"{ColumnLimit: $clangColumnLimit}"
  )
  private val clangColumnLimit                     = 120
  private def clang(noExtension: String)           = clangCmd ++ Seq(s"$noExtension.cpp", "-o", s"$noExtension.out")
  private def run(noExtension: String)             = Seq(s"./$noExtension.out")
  private def cppPath(noExtension: String)         = Paths.get(generatedDir.toString, s"$noExtension.cpp")
  private def getNoExtension(path: String): String = reFilename.findAllIn(path).matchData.next().group(2)
  private val reFilename                           = "^(.+/)*(.+)\\.(.+)$".r

  def cmake(dirPath: Path, fileNames: Array[String]): Unit = {
    val contents = cmakeContents(fileNames.map(dirPath.resolve).map(_.toString).map(getNoExtension).toSeq)
    val path     = Paths.get(generatedDir.toString, cmakeFileName)
    val _        = write(path, contents)
  }
  private def cmakeContents(noExtensions: Seq[String]): String = {
    val init = s"""# auto-generated config - handy for Clion
                  |cmake_minimum_required(VERSION $cmakeVersion)
                  |project(generated)
                  |set(CMAKE_CXX_STANDARD $cppStandard)
                  |
                  |# comment out to debug / set breakpoints in Clion
                  |# note: without -O3 it will be super slow on vectors
                  |set(CMAKE_BUILD_TYPE "Release" CACHE STRING "" FORCE)
                  |set(CMAKE_CXX_FLAGS_RELEASE "${releaseFlags.mkString(" ")}")
                  |
                  |set(CMAKE_CXX_FLAGS "$phmapWarningsFlag")
                  |set(CMAKE_RUNTIME_OUTPUT_DIRECTORY $${CMAKE_CURRENT_SOURCE_DIR})
                  |""".stripMargin
    noExtensions.map(noExtension => s"add_executable($noExtension.out $noExtension.cpp)").mkString(init, "\n", "")
  }
  private val cmakeVersion  = "3.28"
  private val cmakeFileName = "CMakeLists.txt"

  def inGeneratedDir(seq: Seq[String]): ProcessBuilder = Process(seq, generatedDir)
  private val generatedDir                             = new java.io.File("generated")

  private val cppStandard       = 17
  private val releaseFlags      = Seq("-O3", "-march=native", "-mtune=native", "-Wno-narrowing", "-ftree-vectorize")
  private val phmapWarningsFlag = "-Wno-deprecated-builtins"
  val clangCmd: Seq[String]     = Seq("clang++", "--std", s"c++$cppStandard") ++ releaseFlags ++ Seq(phmapWarningsFlag)
}
