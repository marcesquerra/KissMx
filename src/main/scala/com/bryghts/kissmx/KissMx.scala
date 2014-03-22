package com.bryghts

import scala.reflect.macros.Context
import scala.language.experimental.macros
import com.bryghts.kissmx.components.GenericExtractor

package kissmx
{

	trait MultiplexingImplementation[T] {
		def fromMap(m: Map[String, Any]): Option[T]
	}
}

package object kissmx
{
	implicit def multiplexingImplementationFor[T]: MultiplexingImplementation[T] = macro packageIntoImpl[T]

	abstract class MxExtractor[T](implicit ct: MultiplexingImplementation[T]) {
		//def unapply(m: Map[String, Any]): Option[T] = ct.fromMap(m)
		def unapply(a: Any): Option[T] = a match {
			case m: Map[String, Any] =>
				ct.fromMap(m)
			case _ =>
				None
		}
	}

	def packageIntoImpl[T](c1: Context)(implicit t1: c1.WeakTypeTag[T]): c1.Expr[MultiplexingImplementation[T]] = {
		val h = new { val c: c1.type = c1} with  MacroImpl[T]

		h.packageIntoImpl
	}

	private abstract class MacroImpl[T] extends GenericExtractor
  {
		val c: Context

		import c.universe._

		def packageIntoImpl(implicit t: c.WeakTypeTag[T]):c.Expr[MultiplexingImplementation[T]] =
		{
			val m = c.Expr[Map[String, Any]](Ident(newTermName("m")))


			val tmp = packageMap(t.tpe, m).asInstanceOf[c.Expr[T]]

			reify{
				new MultiplexingImplementation[T]() {
					def fromMap(m: Map[String, Any]): Option[T] = {

						Some(tmp.splice)
					}
				}
			}
		}
  }
}
