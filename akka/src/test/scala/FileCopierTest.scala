import java.io.File
import java.net.URL
import java.nio.file.Paths
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
    val files = FileFinder.findFilesAt(Paths.get(location))
    assert(2 === files.size, "wrong number of files found")
  }

  def fileCopyTestDir = {
    // get the uri and chop of the file part (I admit: hacky)
    getClass().getClassLoader().getResource("file-copy-test-dir").toString.substring("file:/".length)
  }

}
