object Main {

  def main(args: Array[String]): Unit = {
    println("Up and running")

    val mc = MyClass(12)

    val x: Any = "This is a text"

    val mc2 = mc.add(123)

    val y: List[Nothing] = List()


    mc2 match {
      case a: List[_] => println("List of some type")
      case _ => println("Some other list")
    }

    println(manOf(mc2))
    println("Mc2 is " + mc2)
    println(s"List is $y of type ${manOf(y)}")
  }

  def manOf[T: Manifest](t: T): Manifest[T] = manifest[T]
}



case class MyClass[T](x: T) {
  def add[U >: T](elem: U) : List[U] = List(x) ++ List(elem)
}

sealed trait StateValue[T] {
  type Tpe

  def value: Tpe
}

case class SingleStateValue[T](value: T) extends StateValue[T] {
  type Tpe = T
}

case class MultiStateValue[T](value: List[T]) extends StateValue[T] {
  type Tpe = List[T]
}


// This is
// data RuleState a = SimpleRuleState a | MultiRuleState [a]
sealed trait RuleState[T] {
  def state: StateValue[T]
  def isAbstract: Boolean
}

case class SimpleRuleState[T](state: SingleStateValue[T]) extends RuleState[T] {
  override val isAbstract = false
}

case class MultiRuleState[T](state: MultiStateValue[T]) extends RuleState[T] {
  override val isAbstract: Boolean = false
}



object Logic {
  // if I want to have a common map I need to give up on types...
  type RuleStateMap[_] = Map[String, RuleState[_]]

}


