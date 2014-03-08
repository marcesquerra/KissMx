package a.b

import org.specs2.mutable._
import org.specs2.ScalaCheck
import com.bryghts.kissmx.KissMx._
import com.bryghts.kissmx.KissMx
import java.util
import com.bryghts.kissjson.parser.Parser
import scala.util.Success

class SomeTest extends Specification with ScalaCheck
{

	object KissParser{
		def unapply(in: String):Option[Any]=Parser(in).toOption
	}

	case class Ooo(a: Int)
	case class Sample(i: Long)
	val tmp: MMM[Sample] = KissMx.packageInto[Sample]

	object SampleMx extends MxExtractor[Sample]


	"Parse booleans correctly" in {

		//val m = Map[String, Any]("la" -> 22, "pablito" -> 345, "ooo" -> Map("a" -> 43), "zz" -> '0', "i" -> BigInt(232), "q" -> 78, "x" -> Array(3, 6, 234), "pepito" -> Array(BigInt(23), 5))


		println(Parser(
			""" {
			"i": 3
		} """))

		val m =
		""" {
			"i": 3
		} """


		m match {
			case KissParser(SampleMx(s)) => println(s)//; println(s.x(1))
			case _ => println("Nooooooo!!!!")
		}

		true mustEqual true
	}

}
