# Scala Web Server

Mini web framework inspired by Flask and Django. Written in Scala with a focus of hiding most of side-effects inside monads with help of [Scalaz](https://scalaz.github.io/) library.

## Getting Started

The easiest way to start is to clone the repository using `git clone`, as it is a whole project with proper  `built.sbt` file provided.

### Prerequisites

The project uses sbt build tool to control all of its dependencies. It is available to download from [sbt official site](https://www.scala-sbt.org).
Then you need to compile and run the project using bash commands:
```
$ sbt
sbt:sws> compile
sbt:sws> run
```
When run, the project should serve example site on `localhost:9999`.

### Creating own website

To start working on a web page you need to create a case object extending `ServerMain` abstract class. It serves as basic entrypoint for our framework.
`ServerMain` provides five fields ready to override:

```scala
urls: List[(String, Controller)]
urlsIO: List[(String, IOController)]
static: Option[(String, String)]
favicon: Option[String]
defaultConfigFile: String
```

The first two are used for routing URIs to proper `Controller` (defined as function typed `(Request, ArgsMap) => Response`, where `ArgsMap` is just a typename for `Map[String, String]`). In addition, `urlsIO` lets you control your `IO` monad yourself.

`static` maps URI to folder with static files on the disk and `favicon` maps to file returned by SWS when asked for a `favicon.ico`. `defaultConfigFile` is used when no configuration file is given as command line argument (if there's no default configuration file either, it uses port 9999).

### Sample "Hello world" code

```scala
case object Hello extends ServerMain {
  val urls = List(("", indexView))
  def indexView(request: Request, args: Map[String, String]): Response =
    HttpResponse("Hello world!")
}
```

More sample code can be found in the `pl.edu.mimuw.testapi.TestMain`.

## Used third-party libraries

SWS proudly uses functional programming libraries:
- [Scalaz](https://scalaz.github.io/)
- [ZIO](https://zio.dev)
