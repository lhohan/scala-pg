import akka.actor.{Props, ActorSystem}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{FunSuite, BeforeAndAfterAll}
import scala.language.postfixOps
import scala.concurrent.duration._

/**
 * User: hanlho
 * DateTime: 26/12/13 15:51
 */
class ToggleTestWithKit(_system: ActorSystem) extends TestKit(_system) with FunSuite with ImplicitSender with BeforeAndAfterAll {

  def this() = this(ActorSystem("ToggleSpec"))

  override def afterAll: Unit = system.shutdown()

  test("basic toggling with TestKit"){
    val toggler = _system.actorOf(Props[Toggle])
    toggler ! "Hello, how are you?"
    expectMsg("Happy")
    toggler ! "Hello, how are you?"
    expectMsg("Sad")
    toggler ! "Unknown"
    expectNoMsg()
  }

}
