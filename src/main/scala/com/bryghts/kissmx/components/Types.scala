package com.bryghts.kissmx.components

import scala.reflect.macros.Context
import java.lang.{Boolean => JBoolean, Integer => JInt, Byte => JByte, Character => JChar, Short => JShort, Long => JLong, Float => JFloat, Double => JDouble}
import java.math.{BigInteger => JBigInt, BigDecimal => JBigDecimal}

trait Types
{
  val c: Context

  import c.universe._

  protected val STRING_TYPE        = typeOf[String]
  protected val BOOLEAN_TYPE       = typeOf[Boolean]
  protected val BYTE_TYPE          = typeOf[Byte]
  protected val CHAR_TYPE          = typeOf[Char]
  protected val SHORT_TYPE         = typeOf[Short]
  protected val INT_TYPE           = typeOf[Int]
  protected val LONG_TYPE          = typeOf[Long]
  protected val FLOAT_TYPE         = typeOf[Float]
  protected val DOUBLE_TYPE        = typeOf[Double]
  protected val BIGINT_TYPE        = typeOf[BigInt]
  protected val BIGDECIMAL_TYPE    = typeOf[BigDecimal]

  protected val JBOOLEAN_TYPE      = typeOf[JBoolean]
  protected val JBYTE_TYPE         = typeOf[JByte]
  protected val JCHAR_TYPE         = typeOf[JChar]
  protected val JSHORT_TYPE        = typeOf[JShort]
  protected val JINT_TYPE          = typeOf[JInt]
  protected val JLONG_TYPE         = typeOf[JLong]
  protected val JFLOAT_TYPE        = typeOf[JFloat]
  protected val JDOUBLE_TYPE       = typeOf[JDouble]
  protected val JBIGINT_TYPE       = typeOf[JBigInt]
  protected val JBIGDECIMAL_TYPE   = typeOf[JBigDecimal]

  protected val ANY_TYPE           = typeOf[Any]

}
