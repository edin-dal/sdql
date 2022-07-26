package sdql
package backend

import ir._
import storage.{ Loader, Table }
import scala.annotation.tailrec

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
    case DictNode(vals) => normalize(vals.map(x => run(x._1) -> run(x._2)).toMap)
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
        case "==" => return equal(v1, v2)
        case "!=" => return !equal(v1, v2)
        case _ => 
      }
      def cmp(d1: Double, d2: Double): Int = {
        @tailrec def findPrec(d: Double, res: Double): Double = 
          if(d <= 0) res else findPrec(d * 10 - (d * 10).toInt, res / 10)
        val precision = findPrec(d2 - d2.toInt, 1) * 0.001
        if(math.abs(d1 - d2) < precision) 0 else if (d1 < d2 + precision) -1 else 1
      }
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
    case Concat(e1, e2) =>
      val (v1, v2) = (run(e1), run(e2))
      (v1, v2) match {
        case (RecordValue(fs1), RecordValue(fs2)) => 
          val (fs1m, fs2m) = fs1.toMap -> fs2.toMap
          val common = fs1.filter(x1 => fs2m.contains(x1._1)).map(x1 => (x1._1, x1._2, fs2m(x1._1)))
          if(common.isEmpty)
            RecordValue(fs1 ++ fs2)
          else
            if(common.forall(x => x._2 == x._3))
              RecordValue(fs1 ++ fs2.filter(x2 => !fs1m.contains(x2._1)))
            else
              raise(s"`concat($v1, $v2)` with different values for the same field name")
        case _ =>
          raise(s"`concat($v1,$v2)` needs records, but given `${v1.getClass}`, `${v2.getClass}`")
      }
    case Sum(k, v, e1, e2) =>
      val v1 = run(e1)
      v1 match {
        case ZeroValue =>
          ZeroValue
        case range: Map[Value, Value] =>
          if(range.isEmpty)
            ZeroValue
          else {
            var res: Value = ZeroValue
            val mutRes = scala.collection.mutable.Map[Value, Value]()
            var inPlace = false
            var firstIter = true
            for(kv <- range) {
              val cur = run(e2)(ctx ++ Map(k -> kv._1, v -> kv._2))
              if(firstIter) {
                firstIter = false
                if(cur.isInstanceOf[Map[_, _]]) {
                  inPlace = true
                }
                else
                  inPlace = false
              }
              if(inPlace)
                inPlaceAdd(mutRes, cur.asInstanceOf[Map[Value, Value]])
              else
                res = add(res, cur)
            }
            if(inPlace)
              mutRes.toMap
            else
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
    case Promote(tp, e1) => 
      val v1 = run(e1)
      def notSupported() = raise(s"`promote[$tp]($v1)` not supported for value of type `${v1.getClass}`")
      tp match {
        case tsrt: TropicalSemiRingType => 
          val d1 = v1 match {
            case d: Double => d
            case i: Int => i.toDouble
            case _ => notSupported()
          }
          TropicalSemiRing(tsrt, d1)
        case etp @ EnumSemiRingType(tp1) =>
          EnumSemiRing(etp, SingletonEnumSemiRing(v1))
        case RealType =>
          v1 match {
            case i: Int => i.toDouble
            case d: Double => d
            case t: TropicalSemiRing[Double] if t.value.nonEmpty => t.value.get
            case et: EnumSemiRing[Double] if et.value.nonEmpty => et.value.get
            case _ => notSupported()
          }
        case IntType =>
          v1 match {
            case i: Int => i
            case d: Double => d.toInt
            case t: TropicalSemiRing[Double] if t.value.nonEmpty  => t.value.get.toInt
            case et: EnumSemiRing[Int] if et.value.nonEmpty => et.value.get
            case _ => notSupported()
          }
        case _ => notSupported()
      }
    case External(name, args) =>
      val vs = args.map(x => run(x)(ctx))
      external(name, vs)
  }
  def equal(v1: Value, v2: Value): Boolean = {
    (v1, v2) match {
      case (ZeroValue, ZeroValue) => true
      case (_, ZeroValue) => equal(v2, v1)
      case (ZeroValue, v2) if isZero(v2) => true
      case _ => v1 == v2
    }
  }
  def isZero(v: Value): Boolean = v match {
    case false => true
    case 0 => true
    case 0.0 => true
    case TropicalSemiRing(_, None) => true
    case EnumSemiRing(_, BottomEnumSemiRing) => true
    case ZeroValue => true
    case x: Map[_, _] if x.isEmpty => true
    case RecordValue(vals) if vals.forall(x => isZero(x._2)) => true
    case _ => false
  }
  def inPlaceAdd(v1: scala.collection.mutable.Map[Value, Value], v2: Map[Value, Value]): Unit = {
    for(x <- v2) {
      val rhs = 
        if(v1.contains(x._1))
          add(v1(x._1), x._2)
        else 
          x._2
      if(!isZero(rhs))
        v1(x._1) = rhs
      else if (isZero(rhs) && v1.contains(x._1))
        v1.remove(x._1)
    }
  }
  def add(v1: Value, v2: Value): Value = {
    (v1, v2) match {
      case (MinSumSemiRing(Some(s1)), MinSumSemiRing(Some(s2))) =>
        MinSumSemiRing(Some(math.min(s1, s2)))
      case (MaxSumSemiRing(Some(s1)), MaxSumSemiRing(Some(s2))) => 
        MaxSumSemiRing(Some(math.max(s1, s2)))
      case (MinProdSemiRing(Some(s1)), MinProdSemiRing(Some(s2))) =>
        MinProdSemiRing(Some(math.min(s1, s2)))
      case (MaxProdSemiRing(Some(s1)), MaxProdSemiRing(Some(s2))) => 
        MaxProdSemiRing(Some(math.max(s1, s2)))
      case (EnumSemiRing(tp1, en1), EnumSemiRing(tp2, en2)) if(tp1 == tp2) => 
        (en1, en2) match {
          case (TopEnumSemiRing, _) | (_, TopEnumSemiRing) => EnumSemiRing(tp1, TopEnumSemiRing)
          case (BottomEnumSemiRing, _) => v2
          case (_, BottomEnumSemiRing) => v1
          case (SingletonEnumSemiRing(s1), SingletonEnumSemiRing(s2)) =>
            if(equal(s1, s2))
              v1
            else
              EnumSemiRing(tp1, TopEnumSemiRing)
          case _ => 
            raise(s"enum addition not supported: `$en1`, `$en2`")
        }
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
        val res = (r1.keys ++ r2.keys).map(k => k -> {
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
        normalize(res)
      case _ => raise(s"`$v1 + $v2` not handled")
    }
  }
  def normalize(v: Value): Value = v match {
    case m: Map[Value, Value] => 
      m.map(kv => kv._1 -> normalize(kv._2)).filter(kv => !isZero(kv._2))
    case RecordValue(vals) =>
      RecordValue(vals.map(v => v._1 -> normalize(v._2)))
    case _ =>
      v
  }
  def mult(v1: Value, v2: Value): Value = {
    (v1, v2) match {
      case (MinSumSemiRing(Some(s1)), MinSumSemiRing(Some(s2))) =>
        MinSumSemiRing(Some(s1 + s2))
      case (MaxSumSemiRing(Some(s1)), MaxSumSemiRing(Some(s2))) => 
        MaxSumSemiRing(Some(s1 + s2))
      case (MinProdSemiRing(Some(s1)), MinProdSemiRing(Some(s2))) =>
        MinProdSemiRing(Some(s1 * s2))
      case (MaxProdSemiRing(Some(s1)), MaxProdSemiRing(Some(s2))) => 
        MaxProdSemiRing(Some(s1 * s2))
      case (EnumSemiRing(tp1, en1), EnumSemiRing(tp2, en2)) if(tp1 == tp2) => 
        (en1, en2) match {
          case (BottomEnumSemiRing, _) | (_, BottomEnumSemiRing) => EnumSemiRing(tp1, BottomEnumSemiRing)
          case (TopEnumSemiRing, _) => v2
          case (_, TopEnumSemiRing) => v1
          case (SingletonEnumSemiRing(s1), SingletonEnumSemiRing(s2)) =>
            if(equal(s1, s2))
              v1
            else
              EnumSemiRing(tp1, BottomEnumSemiRing)
          case _ => 
            raise(s"enum multiplication not supported: `$en1`, `$en2`")
        }
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
    import ExternalFunctions._
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
      case x if x == StrIndexOf.SYMBOL => (args(0), args(1), args(2)) match {
        case (str1: String, str2: String, idx: Int) => str1.indexOf(str2, idx)
        case _ => raiseTp("string, string, int")
      }
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
