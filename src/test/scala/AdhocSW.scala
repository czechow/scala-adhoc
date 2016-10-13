

trait Show[A] {
  def show(v: A): String
}

case class Data()
case class SomeOther(x: Int, y: String)


case class SW[A : Numeric](size: Int) {
  def add(elem: A): A = {
    val im = implicitly[Numeric[A]]
    im.plus(im.zero, elem)
  }
}


object AdhocSW {

  implicit class RightBiasedEither[A, B](e: Either[A, B]) {
    def map[C](f: B => C): Either[A, C] = e.right.map(f)
    def flatMap[C](f: B => Either[A, C]): Either[A, C] = e.right.flatMap(f)
  }

  implicit object DataShow extends Show[Data] {
    def show(v: Data): String = "Data type, no contents"
  }

  implicit object SomeOtherShow extends Show[SomeOther] {
    def show(v: SomeOther): String = s"X is ${v.x}, text is ${v.y}"
  }

  def showMe[T : Show](d: T): Unit = {
    val sh = implicitly[Show[T]]
    println("Showing: " + sh.show(d))
  }

  def double(name: String)(v: AnyRef): Either[String, Double] = v match {
    case d: java.lang.Double => Right(d.doubleValue())
    case _ => Left(s"Incorrect double in field $name")
  }

  def string(name: String)(v: AnyRef): Either[String, String] = v match {
    case s: String => Right(s)
    case _ => Left(s"Incorrect string in field $name")
  }

  def long(name:String)(v: AnyRef): Either[String, Long] = ???

  def fromMM(mm: Map[String, AnyRef]): Either[String, (Option[String], Double, Int)] = for {
    s <- validate(mm, "k1")(_ => v => Right(v.toString))()
    d <- validateMand(mm, "k2")(double)()
  } yield {
    (s, d, 13)
  }

  def mand[T](name: String)(op: Option[T]): Either[String, T] = op.toRight(s"Mand $name missing")


  def validate[T](mm: Map[String, AnyRef], name: String)
                 (fc: String => AnyRef => Either[String, T])
                 (fs: (String => T => Either[String, T]) *)
  : Either[String, Option[T]] =
    mm.get(name) match {
      case Some(v) =>
        fs.foldLeft(fc(name)(v)) { (acc, f) => acc flatMap f(name) } map { r => Some(r) }
      case None => Right(None)
    }

  def validateMand[T](mm: Map[String, AnyRef], name: String)
                     (fc: String => AnyRef => Either[String, T])
                     (fs: (String => T => Either[String, T]) *)
  : Either[String, T] = validate(mm, name)(fc)(fs: _*) flatMap mand(name)

  def main(args: Array[String]): Unit = {
    val mm = Map[String, AnyRef]("k1" -> "Wania", "k2" -> Double.box(234d))
    println(fromMM(mm))
  }

}

