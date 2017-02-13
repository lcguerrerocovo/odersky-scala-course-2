def map[T,U](list: List[T], f: T => U): List[U] = list match {
  case x :: xs => f(x) :: map(xs,f)
  case Nil => Nil
}

def flatMap[T,U](list: List[T], f: T => List[U]): List[U] = list match {
  case x :: xs => f(x) ++ flatMap(xs,f)
  case Nil => Nil
}

def filter[T](list: List[T], f: T => Boolean): List[T] = list match {
  case x :: xs => if(f(x)) x :: filter(xs,f) else filter(xs,f)
  case Nil => Nil
}

val list = List(1,2,3,4,5)

def g = (x: Int) => List(x-1,x,x+1)

assert(map(list,g) == List(List(0, 1, 2), List(1, 2, 3),
  List(2, 3, 4), List(3, 4, 5), List(4, 5, 6)))
assert(flatMap(list,g) == List(0, 1, 2, 1, 2, 3, 2, 3
  , 4, 3, 4, 5, 4, 5, 6))
assert(filter(list,(x: Int) => x < 3) == List(1,2))

// expanding for comprehensions based on the three rules

assert(
  (for {
    x <- 2 to 10
    y <- 2 to x
    if(x % y == 0)
   } yield (x,y))
  ==
    ((2 to 10).flatMap(x =>
      (2 to x).withFilter(y =>
        (x % y == 0)).map(y => (x,y)))))
