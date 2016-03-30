package dk.aau.sw404f16.util
/**
  * Created by coffee on 3/30/16.
  */
object Extensions {
  implicit class RichStringList(val left: List[String]) extends AnyVal {
    def stripSpaces: List[String] = {
      val head :: tail = left
      if(tail != Nil) head match {
        case " " | "" => tail.stripSpaces
        case _   => head :: tail.stripSpaces
      } else left
    }
  }
}
