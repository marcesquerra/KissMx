package com.bryghts.kissmx.components

import scala.reflect.macros.Context


trait Expressions
{
  val c: Context

  import c.universe._


  protected val retNone = c.Expr[Nothing](Return(reify{None}.tree))

  protected def doApplyList(o: Tree, v: Tree): c.Expr[Any] = {
    c.Expr[Any] {
      Apply(
        Select(
          o,
          newTermName("apply")),
        List(
          Typed(
            v, Ident(tpnme.WILDCARD_STAR))))
    }
  }
}
