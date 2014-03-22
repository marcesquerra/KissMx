package com.bryghts.kissmx.components

import scala.reflect.macros.Context
import java.util


trait CollectionExtractor extends Types
                             with Expressions
{self : GenericExtractor =>
	val c: Context

	import c.universe._

	protected def extractCollection(targetType: c.Type, v: c.Expr[Any]): Option[c.Expr[Any]] ={
		if(targetType.typeSymbol.asClass.baseClasses.contains(typeOf[Array[Any]].typeSymbol))
		{
			val vv = c.Expr[Any](Ident(newTermName("vv")))
			val converted = extract(targetType.asInstanceOf[TypeRef].args.head, vv)
			Some(reify{
				v.splice match {
					case a: Traversable[_] =>
						a.map{vv =>
							converted.splice
						}.toArray
					case a: java.lang.Iterable[_] =>
						scala.collection.JavaConversions.iterableAsScalaIterable(a).map{vv =>
							converted.splice
						}.toArray
					case a: Array[_] =>
						a.map{vv =>
							converted.splice
						}
					case _ =>
						retNone.splice
				}
			})
		}
		else if(targetType.typeSymbol.asClass.baseClasses.contains(typeOf[Traversable[Any]].typeSymbol))
		{
			val vv = c.Expr[Any](Ident(newTermName("vv")))
			val converted = extract(targetType.asInstanceOf[TypeRef].args.head, vv)
			val hasCompanionApply =
				targetType
					.typeSymbol
					.companionSymbol
					.typeSignature
					.declarations
					.filter(_.isMethod)
					.filter(_.name.decoded == "apply")
					.map(_.asMethod)
					.filter(_.paramss.length == 1)
					.filter(_.paramss.head.length == 1)
					.filter(_.isVarargs)
					.size == 1

			if(hasCompanionApply){
				val l = reify{
					v.splice match {
						case a: Traversable[_] =>
							a.map{vv =>
								converted.splice
							}.toList
						case a: java.lang.Iterable[_] =>
							scala.collection.JavaConversions.iterableAsScalaIterable(a).map{vv =>
								converted.splice
							}.toList
						case a: Array[_] =>
							a.toList.map{vv =>
								converted.splice
							}
						case _ =>
							retNone.splice
					}
				}

				Some(doApplyList(Ident(targetType.typeSymbol.companionSymbol.asModule), l.tree))
			}
			else
				c.abort(c.enclosingPosition, "Unsuported type " + targetType)
		}
		else if(targetType.typeSymbol.asClass.baseClasses.contains(typeOf[util.Collection[Any]].typeSymbol))
		{
			val vv = c.Expr[Any](Ident(newTermName("vv")))
			val converted = extract(targetType.asInstanceOf[TypeRef].args.head, vv)
			val constructors = targetType
				.declarations
				.toList
				.filter(_.isMethod)
				.map(_.asMethod)
				.filter(_.isConstructor)
				.filter(_.isPublic)
				.filter(_.paramss.flatten.size == 0)

			val tmp = c.Expr[util.Collection[Any]](Apply(Select(New(AppliedTypeTree(Ident(targetType.typeSymbol), List(Ident(targetType.asInstanceOf[TypeRef].args.head.typeSymbol)))), nme.CONSTRUCTOR), List()))

			if(constructors.size == 1){
				Some(reify{
					v.splice match {
						case a: Traversable[_] =>
							val t = tmp.splice
							val sc = a.map{vv => converted.splice}.toList
							t.addAll(scala.collection.JavaConversions.asJavaCollection(sc))
							t
						case a: java.lang.Iterable[_] =>
							val t = tmp.splice
							val sc =
								scala.collection.JavaConversions.iterableAsScalaIterable(a).map{vv =>
									converted.splice
								}.toList
							t.addAll(scala.collection.JavaConversions.asJavaCollection(sc))
							t
						case a: Array[_] =>
							val t = tmp.splice
							val sc = a.toList.map{vv =>
								converted.splice
							}
							t.addAll(scala.collection.JavaConversions.asJavaCollection(sc))
							t
						case _ =>
							retNone.splice
					}
				})
			}
			else
				c.abort(c.enclosingPosition, "Unsuported type " + targetType)
		}
		else None
	}
}
