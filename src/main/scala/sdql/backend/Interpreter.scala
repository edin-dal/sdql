package sdql
package backend

import ir._
import storage.{ Loader, Table }

object Interpreter {
  type Value = Any
  type Var = Sym
  type Ctx = Map[Var, Value]
  def apply(e: Exp): Value = run(e)(Map())
  def run(e: Exp)(implicit ctx: Ctx): Value = e match {
    case Const(v) => v
    case RecNode(vals) => RecordValue(vals.map(x => x._1 -> run(x._2)))
    case sym@Sym(name) => ctx.get(sym) match {
      case Some(v) => v
      case None => raise(s"Variable `$name` not in scope!")
    }
    case DictNode(vals) => vals.map(x => run(x._1) -> run(x._2)).toMap
    case LetBinding(x, e1, e2) =>
      val v1 = run(e1)
      run(e2)(ctx ++ Map(x -> v1))
    case Mult(e1, e2) => 
      val (v1, v2) = (run(e1), run(e2))
      mult(v1, v2)
    case Add(e1, e2) => 
      val (v1, v2) = (run(e1), run(e2))
      add(v1, v2)
    case Neg(e1) =>
      val v1 = run(e1)
      v1 match {
        case ZeroValue => ZeroValue
        case r1: Int => -r1
        case r1: Double => -r1
        case _ => raise(s"`-$v1` not handled")
      }
    case IfThenElse(cond, e1, e2) =>
      val vcond = run(cond)
      vcond match {
        case true => run(e1)
        case false | ZeroValue => run(e2)
        case _ => raise(s"`if($vcond)` not handled")
      }
    case Cmp(e1, e2, op) =>
      val (v1, v2) = (run(e1), run(e2))
      op match {
        case "==" => return v1 == v2
        case "!=" => return v1 != v2
        case _ => 
      }
      def cmp(d1: Double, d2: Double): Int = 
        if(d1 < d2) -1 else if (d1 > d2) 1 else 0 
      val res = (v1, v2) match {
        case (r1: Int, r2: Int) => cmp(r1.toDouble, r2.toDouble)
        case (r1: Int, r2: Double) => cmp(r1.toDouble, r2)
        case (r1: Double, r2: Int) => cmp(r1, r2.toDouble)
        case (r1: Double, r2: Double) => cmp(r1, r2)
        case (DateValue(r1), DateValue(r2)) => cmp(r1.toDouble, r2.toDouble)
        case _ => raise(s"`$v1 $op $v2` not handled")
      }
      op match {
        case "<" => res == -1
        case "<=" => res == -1 || res == 0
        case _ => raise(s"`$v1 $op $v2`'s operator not handled")
      }
    case FieldNode(e1, f) =>
      val v1 = run(e1) 
      v1 match {
        case RecordValue(vals) =>
          vals.find(v => v._1 == f) match {
            case Some((_, v)) => v
            case None => raise(s"`$v1.$f`: not field named `$f`")
          }
        case _ => 
          raise(s"`$v1.$f`: `$v1` is not a record")
      }
    case Get(e1, e2) =>
      val (v1, v2) = (run(e1), run(e2))
      v1 match {
        case m: Map[Value, _] => 
          m.get(v2) match {
            case Some(vv2) => vv2
            case None => ZeroValue
          }
        case _ =>
          raise(s"`$v1($v2)` needs a dictionary but given `${v1.getClass}`")
      }
    case Sum(k, v, e1, e2) =>
      val v1 = run(e1)
      v1 match {
        case range: Map[Value, Value] =>
          if(range.isEmpty)
            ZeroValue
          else {
            var res: Value = ZeroValue
            for(kv <- range) {
              res = add(res, run(e2)(ctx ++ Map(k -> kv._1, v -> kv._2)))
            }
            res
          }
        case _ =>
          raise(s"`sum(<$k,$v> <- $v1) ...` doesn't have a dictionary range: `${v1.getClass}`")
      }
    case Load(path, tp) => 
      tp match {
        case DictType(RecordType(fs), IntType) =>
          val arr = Loader.loadTable(Table(path, fs, path))
          arr.map(x => x -> 1).toMap
        case _ =>
          raise(s"`load[$tp]('${path}')` only supports the type `{ < ... > -> int }`")
      }
    case External(name, args) =>
      val vs = args.map(x => run(x)(ctx))
      external(name, vs)
  }
  def add(v1: Value, v2: Value): Value = {
    (v1, v2) match {
      case (ZeroValue, r2) => r2
      case (r1, ZeroValue) => r1
      case (r1: Int, r2: Int) => r1 + r2
      case (r1: Int, r2: Double) => r1.toDouble + r2
      case (r1: Double, r2: Int) => r1 + r2.toDouble
      case (r1: Double, r2: Double) => r1 + r2
      case (RecordValue(vs1), RecordValue(vs2)) =>
        if(vs1.map(_._1) != vs2.map(_._1))
          raise(s"record addition with incompatible types")
        else
          RecordValue(vs1.zip(vs2).map(x => x._1._1 -> add(x._1._2, x._2._2)))
      case (r1: Map[Value,_], r2: Map[Value,_]) =>
        (r1.keys ++ r2.keys).map(k => k -> {
          (r1.get(k), r2.get(k)) match {
            case (Some(vv1), Some(vv2)) => 
              add(vv1, vv2)
            case (Some(vv1), None) => 
              vv1
            case (None, Some(vv2)) =>
              vv2
            case _ => ???
          }
        }).toMap
      case _ => raise(s"`$v1 + $v2` not handled")
    }
  }
  def mult(v1: Value, v2: Value): Value = {
    (v1, v2) match {
      case (ZeroValue, r2) => ZeroValue
      case (r1, ZeroValue) => ZeroValue
      case (r1: Int, r2: Int) => r1 * r2
      case (r1: Int, r2: Double) => r1 * r2
      case (r1: Double, r2: Int) => r1 * r2
      case (r1: Double, r2: Double) => r1 * r2
      case (r1: Int, r2: Map[Value, _]) => 
        r2.map(kv => kv._1 -> mult(r1, kv._2))
      case (r1: Double, r2: Map[Value, _]) => 
        r2.map(kv => kv._1 -> mult(r1, kv._2))
      case (r1: Map[Value, _], r2) => 
        r1.map(kv => kv._1 -> mult(kv._2, r2))
      case _ => raise(s"`$v1 * $v2` not handled")
    }
  }
  def external(name: String, args: Seq[Value]): Value = {
    import External._
    def raiseTp(tp: String) = raise(s"ext(`$name`, ...) expects $tp, but given: ${args.mkString(", ")}.")
    implicit def bool2double(b: Boolean): Double = if(b) 1 else 0
    name match {
      case x if x == ParseDate.SYMBOL => args(0) match {
        case v: String =>
          val arr = v.split('-')
          val Array(y, m, d) = arr.map(_.toInt)
          DateValue(y * 10000 + m * 100 + d)
        case _ => raiseTp("string")
      }
      case x if x == Year.SYMBOL => args(0) match {
        case DateValue(r) => r / 10000
        case _ => raiseTp("date")
      }
      case x if x == SubString.SYMBOL => (args(0), args(1), args(2)) match {
        case (str: String, s: Int, l: Int) =>
          str.substring(s, s + l)
        case _ => raiseTp("string, int, int")
      }
      case x if x == StrStartsWith.SYMBOL => (args(0), args(1)) match {
        case (str1: String, str2: String) => str1.startsWith(str2)
        case _ => raiseTp("string, string")
      }
      case x if x == StrEndsWith.SYMBOL => (args(0), args(1)) match {
        case (str1: String, str2: String) => str1.endsWith(str2)
        case _ => raiseTp("string, string")
      }
      case x if x == StrContains.SYMBOL => (args(0), args(1)) match {
        case (str1: String, str2: String) => str1.contains(str2)
        case _ => raiseTp("string, string")
      }
      case x if x == StrContainsN.SYMBOL => 
        val as = args.map(_.asInstanceOf[String])
        val (obj, xs) = as.head ->  as.tail
        xs.forall(x => obj.contains(x))
      case x if x == Inv.SYMBOL => args(0) match {
        case r: Int => 1.0 / r
        case r: Double => 1 / r
        case v => raise(s"`inv($v)` not handled")
      }
      case name =>
        raise(s"ext(`$name`, ${args.mkString(",")}) not handled")
    }
  }
}
