import java.nio.file.{Paths, Path}
import java.util.UUID

def take2(x: Long): String = {
  (x % 100).toString.padTo(2, '0')
}

def take2(u: UUID): String = {
  u.toString.take(2)
}

def toPath(x: Long): Path = {
  Paths.get(s"$x")
}

def toPath(u: UUID): Path = {
  Paths.get(u.toString)
}

take2(0)
take2(222)
take2(UUID.fromString("050e27d0-7017-11e4-9803-0800200c9a66"))

case class Ids(id1: Option[Long], id2: Option[UUID])

def doSomething(ids: Ids): String = ids match {
  case Ids(_, Some(uuid)) => take2(uuid)
  case Ids(Some(long), _) => take2(long)
}

def doSomethingElse(ids: Ids) = ids match {
  case Ids(_, Some(uuid)) => toPath(uuid)
  case Ids(Some(long), _) => toPath(long)
}

doSomething(Ids(Some(12345L), None))
doSomethingElse(Ids(Some(12345L), None))






