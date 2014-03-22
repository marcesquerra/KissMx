package com.bryghts.kissmx.components

import scala.reflect.macros.Context
import java.lang.{Boolean => JBoolean, Integer => JInt, Byte => JByte, Character => JChar, Short => JShort, Long => JLong, Float => JFloat, Double => JDouble}
import java.math.{BigInteger => JBigInt, BigDecimal => JBigDecimal}

trait BaseTypesExtractors extends Types with Expressions
{

  val c: Context

  import c.universe._

  private def extractString(v: c.Expr[Any]): c.Expr[Any] =
    reify{
      v.splice match {
        case s: String => s
        case c: Char   => c.toString
        case _ => retNone.splice
      }
    }

  private def extractBoolean(v: c.Expr[Any]): c.Expr[Any] =
    reify{
      v.splice match {
        case b: Boolean => b
        case b: JBoolean => b.booleanValue()
        case _ => retNone.splice
      }
    }

  private def extractJavaBoolean(v: c.Expr[Any]): c.Expr[Any] =
    reify{
      v.splice match {
        case b: Boolean => new JBoolean(b)
        case b: JBoolean => b
        case _ => retNone.splice
      }
    }

  private def extractByte(v: c.Expr[Any]): c.Expr[Any] =
    reify{
      v.splice match {
        case b: Byte       => b
        case c: Char       => c.toByte
        case s: Short      => s.toByte
        case i: Int        => i.toByte
        case l: Long       => l.toByte
        case f: Float      => f.toByte
        case d: Double     => d.toByte
        case b: BigInt     => b.toByte
        case d: BigDecimal => d.toByte

        case b: JByte      => b.byteValue()
        case c: JChar      => c.charValue().toByte
        case s: JShort     => s.shortValue().toByte
        case i: JInt       => i.intValue().toByte
        case l: JLong      => l.longValue().toByte
        case f: JFloat     => f.floatValue().toByte
        case d: JDouble    => d.doubleValue().toByte
        case b: JBigInt    => b.byteValue()
        case d: JBigDecimal=> d.byteValue()

        case _             => retNone.splice
      }
    }

  private def extractChar(v: c.Expr[Any]): c.Expr[Any] =
    reify{
      v.splice match {
        case b: Byte       => b.toChar
        case c: Char       => c
        case s: Short      => s.toChar
        case i: Int        => i.toChar
        case l: Long       => l.toChar
        case f: Float      => f.toChar
        case d: Double     => d.toChar
        case b: BigInt     => b.toChar
        case d: BigDecimal => d.toChar

        case b: JByte      => b.byteValue().toChar
        case c: JChar      => c.charValue()
        case s: JShort     => s.shortValue().toChar
        case i: JInt       => i.intValue().toChar
        case l: JLong      => l.longValue().toChar
        case f: JFloat     => f.floatValue().toChar
        case d: JDouble    => d.doubleValue().toChar
        case b: JBigInt    => b.intValue().toChar
        case d: JBigDecimal=> d.intValue().toChar
        case _             => retNone.splice
      }
    }

  private def extractShort(v: c.Expr[Any]): c.Expr[Any] =
    reify{
      v.splice match {
        case b: Byte       => b.toShort
        case c: Char       => c.toShort
        case s: Short      => s
        case i: Int        => i.toShort
        case l: Long       => l.toShort
        case f: Float      => f.toShort
        case d: Double     => d.toShort
        case b: BigInt     => b.toShort
        case d: BigDecimal => d.toShort

        case b: JByte      => b.byteValue().toShort
        case c: JChar      => c.charValue().toShort
        case s: JShort     => s.shortValue()
        case i: JInt       => i.intValue().toShort
        case l: JLong      => l.longValue().toShort
        case f: JFloat     => f.floatValue().toShort
        case d: JDouble    => d.doubleValue().toShort
        case b: JBigInt    => b.shortValue()
        case d: JBigDecimal=> d.shortValue()
        case _             => retNone.splice
      }
    }

  private def extractInt(v: c.Expr[Any]): c.Expr[Any] =
    reify{
      v.splice match {
        case b: Byte       => b.toInt
        case c: Char       => c.toInt
        case s: Short      => s.toInt
        case i: Int        => i
        case l: Long       => l.toInt
        case f: Float      => f.toInt
        case d: Double     => d.toInt
        case b: BigInt     => b.toInt
        case d: BigDecimal => d.toInt

        case b: JByte      => b.byteValue().toInt
        case c: JChar      => c.charValue().toInt
        case s: JShort     => s.shortValue().toInt
        case i: JInt       => i.intValue()
        case l: JLong      => l.longValue().toInt
        case f: JFloat     => f.floatValue().toInt
        case d: JDouble    => d.doubleValue().toInt
        case b: JBigInt    => b.intValue()
        case d: JBigDecimal=> d.intValue()
        case _             => retNone.splice
      }
    }

  private def extractLong(v: c.Expr[Any]): c.Expr[Any] =
    reify{
      v.splice match {
        case b: Byte       => b.toLong
        case c: Char       => c.toLong
        case s: Short      => s.toLong
        case i: Int        => i.toLong
        case l: Long       => l
        case f: Float      => f.toLong
        case d: Double     => d.toLong
        case b: BigInt     => b.toLong
        case d: BigDecimal => d.toLong

        case b: JByte      => b.byteValue().toLong
        case c: JChar      => c.charValue().toLong
        case s: JShort     => s.shortValue().toLong
        case i: JInt       => i.intValue().toLong
        case l: JLong      => l.longValue()
        case f: JFloat     => f.floatValue().toLong
        case d: JDouble    => d.doubleValue().toLong
        case b: JBigInt    => b.longValue()
        case d: JBigDecimal=> d.longValue()
        case _             => retNone.splice
      }
    }

  private def extractFloat(v: c.Expr[Any]): c.Expr[Any] =
    reify{
      v.splice match {
        case b: Byte       => b.toFloat
        case c: Char       => c.toFloat
        case s: Short      => s.toFloat
        case i: Int        => i.toFloat
        case l: Long       => l.toFloat
        case f: Float      => f
        case d: Double     => d.toFloat
        case b: BigInt     => b.toFloat
        case d: BigDecimal => d.toFloat

        case b: JByte      => b.byteValue().toFloat
        case c: JChar      => c.charValue().toFloat
        case s: JShort     => s.shortValue().toFloat
        case i: JInt       => i.intValue().toFloat
        case l: JLong      => l.longValue().toFloat
        case f: JFloat     => f.floatValue()
        case d: JDouble    => d.doubleValue().toFloat
        case b: JBigInt    => b.floatValue()
        case d: JBigDecimal=> d.floatValue()
        case _             => retNone.splice
      }
    }

  private def extractDouble(v: c.Expr[Any]): c.Expr[Any] =
    reify{
      v.splice match {
        case b: Byte       => b.toDouble
        case c: Char       => c.toDouble
        case s: Short      => s.toDouble
        case i: Int        => i.toDouble
        case l: Long       => l.toDouble
        case f: Float      => f.toDouble
        case d: Double     => d
        case b: BigInt     => b.toDouble
        case d: BigDecimal => d.toDouble

        case b: JByte      => b.byteValue().toDouble
        case c: JChar      => c.charValue().toDouble
        case s: JShort     => s.shortValue().toDouble
        case i: JInt       => i.intValue().toDouble
        case l: JLong      => l.longValue().toDouble
        case f: JFloat     => f.floatValue().toDouble
        case d: JDouble    => d.doubleValue()
        case b: JBigInt    => b.doubleValue()
        case d: JBigDecimal=> d.doubleValue()
        case _             => retNone.splice
      }
    }

  private def extractBigInt(v: c.Expr[Any]): c.Expr[Any] =
    reify{
      v.splice match {
        case b: Byte       => BigInt(b)
        case c: Char       => BigInt(c)
        case s: Short      => BigInt(s)
        case i: Int        => BigInt(i)
        case l: Long       => BigInt(l)
        case f: Float      => BigInt(f.toLong)
        case d: Double     => BigInt(d.toLong)
        case b: BigInt     => b
        case d: BigDecimal => d.toBigInt()

        case b: JByte      => BigInt(b.byteValue())
        case c: JChar      => BigInt(c.charValue())
        case s: JShort     => BigInt(s.shortValue())
        case i: JInt       => BigInt(i.intValue())
        case l: JLong      => BigInt(l.longValue())
        case f: JFloat     => BigInt(f.longValue)
        case d: JDouble    => BigInt(d.longValue)
        case b: JBigInt    => BigInt(b)
        case d: JBigDecimal=> BigDecimal(d).toBigInt()
        case _             => retNone.splice
      }
    }

  private def extractBigDecimal(v: c.Expr[Any]): c.Expr[Any] =
    reify{
      v.splice match {
        case b: Byte       => BigDecimal(b)
        case c: Char       => BigDecimal(c)
        case s: Short      => BigDecimal(s)
        case i: Int        => BigDecimal(i)
        case l: Long       => BigDecimal(l)
        case f: Float      => BigDecimal(f)
        case d: Double     => BigDecimal(d)
        case b: BigInt     => BigDecimal(b)
        case d: BigDecimal => d

        case b: JByte      => BigDecimal(b.byteValue())
        case c: JChar      => BigDecimal(c.charValue())
        case s: JShort     => BigDecimal(s.shortValue())
        case i: JInt       => BigDecimal(i.intValue())
        case l: JLong      => BigDecimal(l.longValue())
        case f: JFloat     => BigDecimal(f.floatValue())
        case d: JDouble    => BigDecimal(d.doubleValue())
        case b: JBigInt    => BigDecimal(b)
        case d: JBigDecimal=> BigDecimal(d)
        case _             => retNone.splice
      }
    }

  private def extractJavaByte(v: c.Expr[Any]): c.Expr[Any] =
    reify{
      v.splice match {
        case b: Byte       => new JByte(b)
        case c: Char       => new JByte(c.toByte)
        case s: Short      => new JByte(s.toByte)
        case i: Int        => new JByte(i.toByte)
        case l: Long       => new JByte(l.toByte)
        case f: Float      => new JByte(f.toByte)
        case d: Double     => new JByte(d.toByte)
        case b: BigInt     => new JByte(b.toByte)
        case d: BigDecimal => new JByte(d.toByte)

        case b: JByte      => b
        case c: JChar      => new JByte(c.charValue().toByte)
        case s: JShort     => new JByte(s.byteValue())
        case i: JInt       => new JByte(i.byteValue())
        case l: JLong      => new JByte(l.byteValue())
        case f: JFloat     => new JByte(f.byteValue())
        case d: JDouble    => new JByte(d.byteValue())
        case b: JBigInt    => new JByte(b.byteValue())
        case d: JBigDecimal=> new JByte(d.byteValue())

        case _             => retNone.splice
      }
    }

  private def extractJavaCharacter(v: c.Expr[Any]): c.Expr[Any] =
    reify{
      v.splice match {
        case b: Byte       => new JChar(b.toChar)
        case c: Char       => new JChar(c)
        case s: Short      => new JChar(s.toChar)
        case i: Int        => new JChar(i.toChar)
        case l: Long       => new JChar(l.toChar)
        case f: Float      => new JChar(f.toChar)
        case d: Double     => new JChar(d.toChar)
        case b: BigInt     => new JChar(b.toChar)
        case d: BigDecimal => new JChar(d.toChar)

        case b: JByte      => new JChar(b.byteValue.toChar)
        case c: JChar      => c
        case s: JShort     => new JChar(s.toChar)
        case i: JInt       => new JChar(i.toChar)
        case l: JLong      => new JChar(l.toChar)
        case f: JFloat     => new JChar(f.toChar)
        case d: JDouble    => new JChar(d.toChar)
        case b: JBigInt    => new JChar(b.intValue().toChar)
        case d: JBigDecimal=> new JChar(d.intValue().toChar)

        case _             => retNone.splice
      }
    }

  private def extractJavaShort(v: c.Expr[Any]): c.Expr[Any] =
    reify{
      v.splice match {
        case b: Byte       => new JShort(b.toShort)
        case c: Char       => new JShort(c.toShort)
        case s: Short      => new JShort(s)
        case i: Int        => new JShort(i.toShort)
        case l: Long       => new JShort(l.toShort)
        case f: Float      => new JShort(f.toShort)
        case d: Double     => new JShort(d.toShort)
        case b: BigInt     => new JShort(b.toShort)
        case d: BigDecimal => new JShort(d.toShort)

        case b: JByte      => new JShort(b.shortValue)
        case c: JChar      => new JShort(c.charValue().toShort)
        case s: JShort     => s
        case i: JInt       => new JShort(i.shortValue())
        case l: JLong      => new JShort(l.shortValue())
        case f: JFloat     => new JShort(f.shortValue())
        case d: JDouble    => new JShort(d.shortValue())
        case b: JBigInt    => new JShort(b.shortValue())
        case d: JBigDecimal=> new JShort(d.shortValue())

        case _             => retNone.splice
      }
    }

  private def extractJavaInteger(v: c.Expr[Any]): c.Expr[Any] =
    reify{
      v.splice match {
        case b: Byte       => new JInt(b.toInt)
        case c: Char       => new JInt(c.toInt)
        case s: Short      => new JInt(s.toInt)
        case i: Int        => new JInt(i)
        case l: Long       => new JInt(l.toInt)
        case f: Float      => new JInt(f.toInt)
        case d: Double     => new JInt(d.toInt)
        case b: BigInt     => new JInt(b.toInt)
        case d: BigDecimal => new JInt(d.toInt)

        case b: JByte      => new JInt(b.intValue)
        case c: JChar      => new JInt(c.charValue().toInt)
        case s: JShort     => new JInt(s.intValue)
        case i: JInt       => i
        case l: JLong      => new JInt(l.intValue())
        case f: JFloat     => new JInt(f.intValue())
        case d: JDouble    => new JInt(d.intValue())
        case b: JBigInt    => new JInt(b.intValue())
        case d: JBigDecimal=> new JInt(d.intValue())

        case _             => retNone.splice
      }
    }

  private def extractJavaLong(v: c.Expr[Any]): c.Expr[Any] =
    reify{
      v.splice match {
        case b: Byte       => new JLong(b.toLong)
        case c: Char       => new JLong(c.toLong)
        case s: Short      => new JLong(s.toLong)
        case i: Int        => new JLong(i.toLong)
        case l: Long       => new JLong(l)
        case f: Float      => new JLong(f.toLong)
        case d: Double     => new JLong(d.toLong)
        case b: BigInt     => new JLong(b.toLong)
        case d: BigDecimal => new JLong(d.toLong)

        case b: JByte      => new JLong(b.longValue)
        case c: JChar      => new JLong(c.charValue().toLong)
        case s: JShort     => new JLong(s.longValue)
        case i: JInt       => new JLong(i.longValue())
        case l: JLong      => l
        case f: JFloat     => new JLong(f.longValue())
        case d: JDouble    => new JLong(d.longValue())
        case b: JBigInt    => new JLong(b.longValue())
        case d: JBigDecimal=> new JLong(d.longValue())

        case _             => retNone.splice
      }
    }

  private def extractJavaFloat(v: c.Expr[Any]): c.Expr[Any] =
    reify{
      v.splice match {
        case b: Byte       => new JFloat(b.toFloat)
        case c: Char       => new JFloat(c.toFloat)
        case s: Short      => new JFloat(s.toFloat)
        case i: Int        => new JFloat(i.toFloat)
        case l: Long       => new JFloat(l.toFloat)
        case f: Float      => new JFloat(f)
        case d: Double     => new JFloat(d.toFloat)
        case b: BigInt     => new JFloat(b.toFloat)
        case d: BigDecimal => new JFloat(d.toFloat)

        case b: JByte      => new JFloat(b.floatValue)
        case c: JChar      => new JFloat(c.charValue().toFloat)
        case s: JShort     => new JFloat(s.floatValue())
        case i: JInt       => new JFloat(i.floatValue())
        case l: JLong      => new JFloat(l.floatValue())
        case f: JFloat     => f
        case d: JDouble    => new JFloat(d.floatValue())
        case b: JBigInt    => new JFloat(b.floatValue())
        case d: JBigDecimal=> new JFloat(d.floatValue())

        case _             => retNone.splice
      }
    }

  private def extractJavaDouble(v: c.Expr[Any]): c.Expr[Any] =
    reify{
      v.splice match {
        case b: Byte       => new JDouble(b.toDouble)
        case c: Char       => new JDouble(c.toDouble)
        case s: Short      => new JDouble(s.toDouble)
        case i: Int        => new JDouble(i.toDouble)
        case l: Long       => new JDouble(l.toDouble)
        case f: Float      => new JDouble(f.toDouble)
        case d: Double     => new JDouble(d)
        case b: BigInt     => new JDouble(b.toDouble)
        case d: BigDecimal => new JDouble(d.toDouble)

        case b: JByte      => new JDouble(b.doubleValue)
        case c: JChar      => new JDouble(c.charValue().toDouble)
        case s: JShort     => new JDouble(s.doubleValue)
        case i: JInt       => new JDouble(i.doubleValue())
        case l: JLong      => new JDouble(l.doubleValue())
        case f: JFloat     => new JDouble(f.doubleValue())
        case d: JDouble    => d
        case b: JBigInt    => new JDouble(b.doubleValue())
        case d: JBigDecimal=> new JDouble(d.doubleValue())

        case _             => retNone.splice
      }
    }

  private def extractJavaBigInteger(v: c.Expr[Any]): c.Expr[Any] =
    reify{
      v.splice match {
        case b: Byte       => BigInt(b).bigInteger
        case c: Char       => BigInt(c).bigInteger
        case s: Short      => BigInt(s).bigInteger
        case i: Int        => BigInt(i).bigInteger
        case l: Long       => BigInt(l).bigInteger
        case f: Float      => BigInt(f.toLong).bigInteger
        case d: Double     => BigInt(d.toLong).bigInteger
        case b: BigInt     => b.bigInteger
        case d: BigDecimal => d.toBigInt().bigInteger

        case b: JByte      => BigInt(b.byteValue()).bigInteger
        case c: JChar      => BigInt(c.charValue()).bigInteger
        case s: JShort     => BigInt(s.shortValue()).bigInteger
        case i: JInt       => BigInt(i.intValue()).bigInteger
        case l: JLong      => BigInt(l.longValue()).bigInteger
        case f: JFloat     => BigInt(f.longValue).bigInteger
        case d: JDouble    => BigInt(d.longValue).bigInteger
        case b: JBigInt    => b
        case d: JBigDecimal=> BigDecimal(d).toBigInt().bigInteger
        case _             => retNone.splice
      }
    }

  private def extractJavaBigDecimal(v: c.Expr[Any]): c.Expr[Any] =
    reify{
      v.splice match {
        case b: Byte       => BigDecimal(b).bigDecimal
        case c: Char       => BigDecimal(c).bigDecimal
        case s: Short      => BigDecimal(s).bigDecimal
        case i: Int        => BigDecimal(i).bigDecimal
        case l: Long       => BigDecimal(l).bigDecimal
        case f: Float      => BigDecimal(f).bigDecimal
        case d: Double     => BigDecimal(d).bigDecimal
        case b: BigInt     => BigDecimal(b).bigDecimal
        case d: BigDecimal => d.bigDecimal

        case b: JByte      => BigDecimal(b.byteValue()).bigDecimal
        case c: JChar      => BigDecimal(c.charValue()).bigDecimal
        case s: JShort     => BigDecimal(s.shortValue()).bigDecimal
        case i: JInt       => BigDecimal(i.intValue()).bigDecimal
        case l: JLong      => BigDecimal(l.longValue()).bigDecimal
        case f: JFloat     => BigDecimal(f.floatValue()).bigDecimal
        case d: JDouble    => BigDecimal(d.doubleValue()).bigDecimal
        case b: JBigInt    => BigDecimal(b).bigDecimal
        case d: JBigDecimal=> d
        case _             => retNone.splice
      }
    }

  private def extractAny(v: c.Expr[Any]): c.Expr[Any] =
    reify{
      v.splice
    }

  protected def extractBaseTypes(targetType: c.Type, v: c.Expr[Any]): Option[c.Expr[Any]] =
    targetType match {
      case STRING_TYPE      => Some(extractString(v))
      case BOOLEAN_TYPE     => Some(extractBoolean(v))
      case JBOOLEAN_TYPE    => Some(extractJavaBoolean(v))
      case BYTE_TYPE        => Some(extractByte(v))
      case CHAR_TYPE        => Some(extractChar(v))
      case SHORT_TYPE       => Some(extractShort(v))
      case INT_TYPE         => Some(extractInt(v))
      case LONG_TYPE        => Some(extractLong(v))
      case FLOAT_TYPE       => Some(extractFloat(v))
      case DOUBLE_TYPE      => Some(extractDouble(v))
      case BIGINT_TYPE      => Some(extractBigInt(v))
      case BIGDECIMAL_TYPE  => Some(extractBigDecimal(v))
      case JBYTE_TYPE       => Some(extractJavaByte(v))
      case JCHAR_TYPE       => Some(extractJavaCharacter(v))
      case JSHORT_TYPE      => Some(extractJavaShort(v))
      case JINT_TYPE        => Some(extractJavaInteger(v))
      case JLONG_TYPE       => Some(extractJavaLong(v))
      case JFLOAT_TYPE      => Some(extractJavaFloat(v))
      case JDOUBLE_TYPE     => Some(extractJavaDouble(v))
      case JBIGINT_TYPE     => Some(extractJavaBigInteger(v))
      case JBIGDECIMAL_TYPE => Some(extractJavaBigDecimal(v))
      case ANY_TYPE         => Some(extractAny(v))
      case _                => None
    }

}
