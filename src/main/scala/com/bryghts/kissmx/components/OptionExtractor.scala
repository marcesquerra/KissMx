package com.bryghts.kissmx.components

import scala.reflect.macros.Context

trait OptionExtractor extends Types
                         with Expressions
{self : GenericExtractor =>
  val c: Context

  import c.universe._

  protected def extractOption(targetType: c.Type, v: c.Expr[Any]): Option[c.Expr[Any]] =
    if(targetType.typeSymbol.asClass.baseClasses.contains(typeOf[Option[Any]].typeSymbol))
    {
      val extracted = extract(targetType.asInstanceOf[TypeRef].args.head, v)

      Some(reify{
        if(v.splice == null)
          None
        else
          Some(extracted.splice)
      })
    }
    else None

}
