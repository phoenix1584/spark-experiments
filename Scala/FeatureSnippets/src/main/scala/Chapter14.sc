import org.scalatest.FunSuite
import org.scalatest.FlatSpec
import org.scalatest.Matchers

class SampleSuite extends FunSuite{
  test("sample test case should pass") {
    assert(1 < 2)
  }
}

(new SampleSuite).execute()

assert(List(1,2,3).contains(4))

class SmapleSuite1 extends FlatSpec with Matchers{
  

}