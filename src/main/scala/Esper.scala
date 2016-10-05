import com.espertech.esper.client._
import com.espertech.esper.client.deploy.DeploymentOptions
import scala.collection.JavaConverters._

import scala.util.Random


object Esper{
  def genRandomTick(cepRT: EPRuntime): Unit = {
    val tick = new Tick("AAPL",Random.nextInt(10),System.currentTimeMillis())
    println(s"Sending tick $tick")
    cepRT.sendEvent(tick)
  }



  def main(args: Array[String]): Unit = {
    val cepConfig = new Configuration()
    cepConfig.addEventType("StockTick", classOf[Tick].getName)
//    cepConfig.addEventType("UnknownTick", classOf[Tick2].getName)
    val cep = EPServiceProviderManager.getProvider("myCEPEngine", cepConfig)


    val cepRT = cep.getEPRuntime
    val cepAdm = cep.getEPAdministrator


    val cepDep = cepAdm.getDeploymentAdmin
    val module = cepDep.read("./main.epl")
    val depRes = cepDep.deploy(module, new DeploymentOptions)
    val stmts = depRes.getStatements.asScala


    val s = stmts.last /// foreach { s =>
    s.addListener(listener)
    //println(s"[${s.getText}]")
    ///}

    1 to 3 foreach { _ => genRandomTick(cepRT) }
  }



  val listener = new UpdateListener {
    var cnt = 1

    override def update(newData: Array[EventBean], oldData: Array[EventBean]): Unit = {
      //println("New data len:"  + newData.length)
      newData foreach { d =>
        println(s"[$cnt] Event received: " + d.getUnderlying)
      }
      cnt = cnt + 1
    }
  }
}






