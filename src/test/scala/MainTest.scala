import org.scalatest.WordSpec

class MainTest extends WordSpec {
  "splitWith" should {
    val args = "--p1" :: "p1v" :: "--p2" :: Nil
    "work correctly" in {
      assert(Main2.splitWith { (x: String) => true } (List()) == List())
      assert(Main2.splitWith { (x: String) => ! x.startsWith("--") } (args)
        == List(List("--p1", "p1v"), List("--p2")))
    }
  }

  "parseCmdLine" should {
    val args = "--p1" :: "p1v" :: "--p2" :: "p2v" :: Nil
    "work correctly" in {
      println(Main2.parseCmdLine(args))
    }
  }


}
