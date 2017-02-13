// implementing new control structure described in lesson 3.3

class REPEAT(command : => Unit) {

  def UNTIL(condition : => Boolean): Unit =  {
    if(condition) ()
    else {command; this UNTIL condition}
  }
}

object REPEAT {
  def apply(command : => Unit): REPEAT = {
    new REPEAT(command)
  }
}


var i = 0;

REPEAT ({i+=1;println("I am repeating")}) UNTIL (i== 3)
