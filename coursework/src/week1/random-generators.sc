import java.util.Random

trait Generator[+T] {
  self =>

  def generate: T

  def map[U](f: (T => U)): Generator[U] = new Generator[U] {
    def generate = f(self.generate)
  }

  def flatMap[U](f: (T => Generator[U])): Generator[U] = new Generator[U] {
    def generate = f(self.generate).generate
  }
}

val integers = new Generator[Int] {
  val rand = new Random
  def generate = rand.nextInt()
}

def single[T](x: T) = new Generator[T] {
  def generate = x
}

def choose(low: Int, high: Int) : Generator[Int] = {
  for(x <- integers) yield low + math.abs(x % (high - low))
}

def oneOf[T](xs: T*): Generator[T] = {
  for(idx <- choose(0,xs.length)) yield xs(idx)
}

val booleans = integers.map(_ > 0)

trait Tree

case class Inner(left: Tree, right: Tree, x: Int) extends Tree

case class Leaf(x: Int) extends Tree

// we will generate a tree randomly using boolean and integer generators
// and then print the generated tree to stdout, the tree will only be printed
// if it has less than depth 6 or less than 128 elements

def treeGenerator: Tree = {
  def treeOrLeaf(choice: Boolean): Tree = {
    if(choice) Leaf(choose(0,10).generate)
    else Inner(treeOrLeaf(booleans.generate),treeOrLeaf(booleans.generate),choose(0,10).generate)
  }
  treeOrLeaf(booleans.generate)
}

def generatePowers(max: Int) = (Iterator.iterate(1)(_ * 2) takeWhile (_ < ((max+1) * 2))).toList

def treePrinter(tree: Tree): Unit = {
  def createTuplesFromTree(tree: Tree, level: Int): List[(Int,String)] = tree match {
    case Inner(left,right,x) =>  {
      (level -> x.toString) :: (createTuplesFromTree(left,level*2) :::
        createTuplesFromTree(right,level*2+1))
    }
    case Leaf(x) => (level -> x.toString) :: Nil
  }

  // FIXME the following code to print the tree is very imperative like :(
  val tupleList = createTuplesFromTree(tree,1)
  val max = tupleList.map(xy => xy._1).max
  val tupleMap = tupleList.toMap withDefaultValue("*")
  val powers = generatePowers(max)
  val powerMax = if(powers.length > 1) powers.filter(_ > max)(0) else max

  if(powerMax < math.pow(2,6)+1) {
    var spaces = powerMax - 1
    for (i <- 1 to max) {
      print((" " * spaces) + {
        if (tupleMap(i) == "*") "   "
        else "[" + tupleMap(i) + "]"
      } +
        (" " * (spaces - 1)))
      if (powers.contains(i + 1)) {
        spaces /= 2
        print("\n")
      }
    }
  } else println("will not print tree with depth greater than log(max)-1 where max is 128")

}

assert(generatePowers(63).last == 64)
treePrinter(treeGenerator)

