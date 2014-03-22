package com.bryghts.kissmx.components

import scala.reflect.macros.Context
import java.util

trait GenericExtractor extends Types
                          with Expressions
                          with BaseTypesExtractors
                          with OptionExtractor
                          with CollectionExtractor
{
  val c: Context

  import c.universe._

  protected def extract(targetType: c.Type, v: c.Expr[Any]): c.Expr[Any] ={

    extractBaseTypes (targetType, v) orElse
    extractOption    (targetType, v) orElse
    extractCollection(targetType, v) getOrElse {
          val vv = c.Expr[Map[String, Any]](Ident(newTermName("vv")))

          val r = packageMap(targetType, vv)

          reify {
            v.splice match {
              case vv: Map[String, Any] =>
                r.splice
              case _ =>
                retNone.splice
            }
          }
    }

  }

  def packageMap(targetType: c.Type, v: c.Expr[Map[String, Any]]):c.Expr[Any] =
  {
    val constructors = targetType.declarations.toList.filter(_.isMethod).map(_.asMethod).filter(_.isConstructor).filter(_.isPublic)

    if(constructors.length != 1)
      c.abort(c.enclosingPosition, "Only classes with one (and only one) constructor can be packaged with KissMx")

    val constructor = constructors.head



    val defaultValues: Map[Int, String] =
      targetType
        .typeSymbol
        .companionSymbol
        .typeSignature
        .declarations
        .toList
        .map(_.name.encoded)
        .filter(_.contains("$default$"))
        .map{n =>
        (n.split("\\$").toList.last.toInt - 1,n)
      }
        .toMap

    val params = constructor.paramss.flatten.zipWithIndex.map{case (p, i) =>

      val n              = c.Expr[String](Literal(Constant(p.name.decoded)))
      val converted      = extract(p.typeSignatureIn(targetType), reify{v.splice(n.splice)})
      val nullConverted  =
        if(p.isTerm && p.asTerm.isParamWithDefault)
        {
          val tmp = defaultValues(i)
          c.Expr[Any](Select(Ident(newTermName(targetType.typeSymbol.companionSymbol.name.encoded)), newTermName(tmp)))
        }
        else
          extract(p.typeSignatureIn(targetType), reify{null: Any})

      reify{
        if(v.splice.contains(n.splice))
        {converted.splice}
        else
        {nullConverted.splice}
      }.tree
    }

    c.Expr[Any](Apply(
    Select(
      New(
        Ident(targetType.typeSymbol)
      ),
      nme.CONSTRUCTOR
    ), {

      params

    }))
  }

}
