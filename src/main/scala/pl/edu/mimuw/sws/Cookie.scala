package pl.edu.mimuw.sws

import java.util.{Date, Locale, TimeZone}
import java.text.SimpleDateFormat
import scalaz._
import Scalaz._

case class Cookie(key: String, value: String, expires: Option[Date], domain: Option[String], path: Option[String],
                  maxAge: Option[Int], secure: Boolean, httpOnly: Boolean) {
  private val DATE_PATTERN = "EEE, dd MMM yyyy HH:mm:ss zzz"
  private val GMT = TimeZone.getTimeZone("GMT")

  private def formatDate(date: Option[Date]): Option[String] = date map {
    val formatter = new SimpleDateFormat(DATE_PATTERN, Locale.US)
    formatter.setTimeZone(GMT)
    formatter.format
  }

  private def parameters: List[String] = (Map[String, Option[String]](
    "Expires" -> formatDate(expires), "Domain" -> domain, "Path" -> path, "Max-Age" -> (maxAge map {_.toString})
  ) flatMap {
    case (k, Some(v)) => (k + "=" + v).some
    case (_, None) => none[String]
  }).toList

  override def toString: String = {
    key + "=" + value +
      parameters.foldLeft("") {(acc, v) => acc + "; " + v} +
      (if (secure) "; Secure" else "") +
      (if (httpOnly) "; HttpOnly" else "")
  }
}

object Cookie {
  def apply(key: String,
            value: String,
            expires: Option[Date] = None,
            domain: Option[String] = None,
            path: Option[String] = None,
            maxAge: Option[Int] = None,
            secure: Boolean = false,
            httpOnly: Boolean = true): Cookie = new Cookie(key, value, expires, domain, path, maxAge, secure, httpOnly)
}