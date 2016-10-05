import scala.reflect.runtime.{universe => ru}


final case class Config(p1: String,
                        p2: Option[String],
                        p3: Double,
                        p4: Option[Double])

case class Param(name: String, tpe: String, mandatory: Boolean)

object Main2 {

  def getParams[A](tt: ru.TypeTag[A]): Either[String, List[Param]] = {
    val t = tt.tpe

    val constructors = t.members filter { s =>
      // FIXME: string comparision is dirty here
      s.isConstructor && s.isPublic && s.name.toString == "<init>"
    }

    val errOrParamLists: Either[String, List[List[ru.Symbol]]] = constructors.toList match {
      case Nil => Left("No default constructor found")
      case (_ :: _ :: _) => Left("More than one default constructor found")
      case (c :: Nil) => Right(c.asMethod.paramLists)
    }

    // FIXME: falling to string tuples here is kind of dirty...
    val errOrParamsTypes = errOrParamLists.right flatMap {
      case Nil =>
        Left("Unable to detect parameters, parameter lists is empty")
      case (_ :: _ :: _) =>
        Left("Multiple parameter lists constructors not supported")
      case (pl :: Nil) =>
        Right(pl map { p => (p.name.toString, p.typeSignature.toString) })
    }

    errOrParamsTypes.right flatMap { pts =>
      sequenceM(pts map { pt => toParam(pt) })
    }
  }

  def main(args: Array[String]): Unit = {
    val args = Array("t1", "t2", "--p1", "p1v", "--p2", "p2v")

    parseCmdLine(args.toList) match {
      case Right(config) => println(s"Config: $config")
      case Left(e) => println(s"Error: $e")
    }
  }


  def splitWith[A](f: A => Boolean)(xs: List[A]): List[List[A]] = xs match {
    case Nil => Nil
    case (y :: ys) =>
      val (as, bs) = ys span f
      (y :: as) :: splitWith(f)(bs)
  }


  def sequenceM[A, B](le: List[Either[A, B]]): Either[A, List[B]] = le match {
    case Nil => Right(List())
    case (Left(e) :: xs) => Left(e)
    case (Right(x) :: xs) => sequenceM(xs) match {
      case Left(e2) => Left(e2)
      case Right(ys) => Right(x :: ys)
    }
  }

  def parseParams(args: List[ClTokenStream], configParams: List[Param])
  : Either[String, (Map[String, List[String]], List[String])] = {
    // This sucks... We need to specify full types here
    val z: Either[String, (Map[String, List[String]], List[String])] =
      Right[String, (Map[String, List[String]], List[String])](Map(), List())

    val configParamsMap =
      (configParams map { case Param(n, t, mandatory) => (n, t) }).toMap

    args.foldLeft(z) {
      case (Left(e), _) => Left(e)
      case (Right((m, rest)), el) =>
        el match {
          case ClArgStream(s) => Right(m, rest ++ s)
          case ClParamArgStream(p, s) =>
            configParamsMap.get(p) match {
              case None => Left(s"Incorrect parameter [$p]")
              case Some(tpe) =>
                paramArgArity.get(tpe) match {
                  case None => Left(s"[ImplErr] Unable to get arity of parameter [$p]") // FIXME: encode in types...
                  case Some(arity) =>
                    val pargs = s.take(arity)
                    if (pargs.length != arity) Left(s"Not enough arguments for parameter [$p]")
                    else Right(m.updated(p, pargs), rest ++ s.drop(arity)) // This update is destructive...
                }
            }
        }
    }
  }

  sealed trait ClTokenStream
  final case class ClArgStream(s: List[String]) extends ClTokenStream
  final case class ClParamArgStream(p: String, s: List[String]) extends ClTokenStream

  def parseCmdLine(args: List[String]): Either[String, (Config, List[String])] = {
    val tokenStreams = splitWith[String](x => ! x.startsWith("--"))(args)

    val clArgStreams: List[ClTokenStream] = tokenStreams match {
      case Nil => throw new Exception(s"[ImplErr] Empty token list for args: [$args]")
      case xs => xs map {
        case Nil => throw new Exception(s"[ImplErr] Empty parameters for token streams [$xs]")
        case (p :: pargs) => ClParamArgStream(p.replaceFirst("--", ""), pargs)
      }
    }

    for {
      configParams <- getParams(ru.typeTag[Config]).right
      paramsAndRest <- parseParams(clArgStreams, configParams).right
      // now -> walk along the parameters, parsing them all, return
    } yield {
      println("PARAMS: " + paramsAndRest)
      Thread.sleep(100)
      ???
    }
  }

  val allowedTypes = Set("String", "Boolean", "Integer", "Double")
  val paramArgArity = (allowedTypes.toList zip List(1, 0, 1, 1)).toMap


  def typeAllowed(st: String): Either[String, String] =
    if (allowedTypes.contains(st)) Right(st)
    else Left(s"Type [$st] not supported")

  def bimap[A, B, C](f: A => A)(g: B => C)(e: Either[A, B]): Either[A, C] = e match {
    case Left(l) => Left(f(l))
    case Right(r) => Right(g(r))
  }

  def toParam(nameType: (String, String)): Either[String, Param] = {
    val (name, tpe) = nameType
    val tokens = tpe.split("[\\[\\]]").toList

    tokens match {
      case ("Option" :: st :: Nil) => bimap[String, String, Param] { err =>
        s"Expression $tpe: $err" } { validType =>
        Param(name, validType, mandatory = false) } (typeAllowed(st))

      case (st :: Nil) => typeAllowed(st).right map {validType =>
        Param(name,validType, mandatory = true)}

      case _ => Left(s"Type not supported: [$tpe]")
    }
  }
}