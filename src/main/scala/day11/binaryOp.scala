package day11

object BinaryOperation {
  def build(opName: String, param1: String, param2: String) = {
    val paramFns = List(param1, param2).map(_ match {
      case "old" => (x: Long) => x
      case v => {
        val va = v.toLong
        (x: Long) => va
      }
    })

    opName match {
      case "+" => (x: Long) => paramFns(0)(x) + paramFns(1)(x)
      case "*" => (x: Long) => paramFns(0)(x) * paramFns(1)(x)
    }
  }
}
