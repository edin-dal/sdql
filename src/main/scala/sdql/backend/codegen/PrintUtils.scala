package sdql.backend.codegen

import sdql.ir.*
import sdql.raise

object PrintUtils {
  def cppPrintResult(tpe: Type): String = tpe match {
    case DictType(kt, vt, NoHint) =>
      s"""for (const auto &[key, val] : $resultName) {
         |$stdCout << ${_cppPrintResult(kt, "key")} << ":" << ${_cppPrintResult(vt, "val")} << std::endl;
         |}""".stripMargin
    case DictType(kt, vt, _: SmallVecDict | VecDict) =>
      assert(vt == IntType, s"${vt.simpleName} != ${IntType.simpleName}")
      val cond = vt match {
        case _: DictType =>
          s"!$resultName[i].empty()"
        case _: RecordType =>
          raise(s"Print not implemented for ${vt.simpleName} inside vector")
        case t =>
          assert(t.isScalar)
          s"$resultName[i] != 0"
      }
      s"""for (auto i = 0; i < $resultName.size(); ++i) {
         |if ($cond) {
         |$stdCout << ${_cppPrintResult(kt, "i")} << ":" << ${_cppPrintResult(vt, s"$resultName[i]")} << std::endl;
         |}
         |}""".stripMargin
    case _ =>
      s"$stdCout << ${_cppPrintResult(tpe, resultName)} << std::endl;"
  }

  private def _cppPrintResult(tpe: Type, name: String) = tpe match {
    case _: DictType =>
      // we currently don't pretty print inside nested dicts
      name
    case RecordType(Nil) =>
      name
    case RecordType(attrs) =>
      attrs.zipWithIndex
        .map(
          {
            case (Attribute(_, tpe: RecordType), _) =>
              raise(s"Nested ${tpe.simpleName} not supported")
            case (Attribute(_, DateType), i) =>
              s"print_date(std::get<$i>($name))"
            case (Attribute(_, _), i) =>
              s"std::get<$i>($name)"
          }
        )
        .mkString(""""<" <<""", """ << "," << """, """<< ">"""")
    case _ =>
      assert(tpe.isScalar)
      name
  }

  private val stdCout = s"std::cout << std::setprecision (std::numeric_limits<double>::digits10)"
}
