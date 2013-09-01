import java.io.File
import java.net.URL
import org.scalatest.FunSuite

/**
 * User: hanlho
 * DateTime: 1/09/13 18:05
 */
class FileCopierTest extends FunSuite {

  import FileCopier.FileFinder

  // FileFinder
  test("test FileFinder on local dir") {
    val location: String = fileCopyTestDir + "/src"
    val filesAt: Array[String] = FileFinder.findFilesAt(location)
    assert(2 === filesAt.size, "wrong number of files found")
  }

  def fileCopyTestDir = {
    // get the uri and chop of the file part (I admit: hacky)
    getClass().getClassLoader().getResource("file-copy-test-dir").toString.substring("file:/".length)
  }

}
