import akka.actor.{Props, ActorSystem}
import akka.testkit.TestProbe
import org.scalatest.FunSuite
import scala.language.postfixOps
import scala.concurrent.duration._

/**
 * User: hanlho
 * DateTime: 26/12/13 15:17
 */
class ToggleTest extends FunSuite{

  test("basic toggling with TestProbe"){
    implicit val system = ActorSystem("TestSystem")
    val toggler = system.actorOf(Props[Toggle])
    val p = TestProbe()
    p.send(toggler, "Hello, how are you?")
    p.expectMsg("Happy" )
    p.send(toggler, "Hello, how are you?")
    p.expectMsg("Sad" )
    p.send(toggler, "Hello, how are you?")
    p.expectMsg("Happy" )
    p.send(toggler, "Unknown")
    p.expectNoMsg()
    system.shutdown()
  }

}
