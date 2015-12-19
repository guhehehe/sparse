package sparse

import java.net.URI

import org.joda.time.{DateTimeZone, DateTime}

import scala.util.{Failure, Success, Try}

object Preamble {
  implicit def Str2Uri(s: String): URI = Try(new URI(s)) match {
    case Success(uri) => Option(uri.getScheme()) match {
      case Some(_) => uri
      case _ => throw new IllegalArgumentException(s"Failed to convert $s to URI: no scheme found.")
    }
    case Failure(e) => throw e
  }

  implicit def Str2Boolean(s: String): Boolean = s.toBoolean

  implicit def Str2Double(s: String): Double = augmentString(s).toDouble

  implicit def Str2Int(s: String): Int = augmentString(s).toInt

  implicit def Str2Set(s: String): Set[String] = Set(s.split(","): _*)

  implicit def Str2Map(s: String): Map[String, String] = {
    Map(s.split(",")
        .map(_.split("="))
        .map { case Array(a1, a2) => (a1, a2) }: _*)
  }

  implicit def Str2DateTime(s: String): DateTime = (new DateTime(s)).toDateTime(DateTimeZone.UTC)
}
