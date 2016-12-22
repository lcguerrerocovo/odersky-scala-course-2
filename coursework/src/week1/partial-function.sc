val f: String => String =  { case "ping" => "pong" }

assert(f("ping") == "pong")

val pingF: PartialFunction[String, String] =  { case "ping" => "pong" }

assert(pingF("ping") == "pong")
assert(pingF.isDefinedAt("ping") == true)
assert(pingF.isDefinedAt("abc") == false)

