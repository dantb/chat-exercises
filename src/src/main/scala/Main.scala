import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.syntax.all.*
import files.FileService
import web.WebService
import openai.GptService
import org.jsoup.Jsoup
import fs2.io.file.Path
import unindent.*
import scala.concurrent.duration.*
import cats.data.*

// Replace the body of Main.run as follows:
// - Accept the URL of a web page as a command line parameter
// - Use WebService to extract the text of the page
// - Use GptService to summarize the text
// - More exercises to follow!

object Main extends IOApp.Simple:
  override def run: IO[Unit] =
    (for
      files <- FileService.resource
      web   <- WebService.resource
      key <- Resource.eval(IO(sys.env.get("GPT_KEY").getOrElse(throw Exception("Missing GPT_KEY"))))
      gpt   <- GptService.resource(key)
    yield (files, web, gpt)).use { (files, web, gpt) =>
      for
        lines   <- files.lines(Path("/Users/DaniTatt/code/learning/chat-exercises/urls.txt"))
        _       <- IO.println(s"Read lines: $lines")
        outputs <- lines
                     .map(line =>
                       IorT.right(IO.println(s"Calling $line")) *>
                         IorT(web.text(line).attempt.map(Ior.fromEither(_)).timeout(3.seconds)).leftMap(NonEmptyList.of(_))
                     )
                     .reduce(_ |+| _).value
        _       <- IO.println(s"Outputs from urls.txt: ${outputs}") // .map(_.map(_.take(50)))}")
      // _ <- printGptOutput(gpt, "Can consciousness emerge in-silico")
      yield ()
    }

  def printGptOutput(client: GptService, msg: String): IO[Unit] =
    client.send(List(msg)).flatMap { result =>
      IO.println(s"ChatGPT output: $result")
    }
