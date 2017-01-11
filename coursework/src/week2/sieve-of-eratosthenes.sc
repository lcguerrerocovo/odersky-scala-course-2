
def from(n: Int) : Stream[Int] = n #:: from(n+1)

(from(0) take 10).toList

def sieve : Stream[Int] = {
  def realSieve(n: Int, primes: List[Int]): Stream[Int] = {
    if(primes filter (n % _ == 0) isEmpty) n #:: realSieve(n+1, n :: primes)
    else realSieve(n+1,primes)
  }
  2 #:: realSieve(3,List(2))
}

(sieve take 100).toList