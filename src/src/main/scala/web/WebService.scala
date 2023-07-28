package web

import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.syntax.all.*
import org.jsoup.Jsoup
import unindent.*

class WebService:
  def text(url: String): IO[String] =
    IO.blocking(Jsoup.connect(url).get().text())

object WebService:
  def resource: Resource[IO, WebService] =
    Resource.pure(WebService())
