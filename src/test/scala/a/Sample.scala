package a

import com.bryghts.kissmx._

case class S(a: Int, b: String, c: List[Option[T]])
case class T(a: Int, b: String)

object S extends MxExtractor[S]

/**
 * Created by dunlord on 16/03/2014.
 */
object Sample extends App{
  Map("a" -> 3, "b" -> "Hello", "c" -> Array(null, Map("a" -> 2, "b" -> "high"))) match {
    case S(result) => println(result)
    case _ => println("Noooo!!!!")
  }

	val tmp:MultiplexingImplementation[S] = multiplexingImplementationFor[S]
}
