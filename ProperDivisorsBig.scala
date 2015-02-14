import annotation.tailrec
import collection.parallel.mutable.ParSeq

def factorize(n: Long): List[Long] = {
    @tailrec
    def factors(tuple: (Long, Long, List[Long], Int)): List[Long] = {
        tuple match {
            case (1, _, acc, _)                 => acc
            case (n, k, acc, _) if (n % k == 0) => factors((n / k, k, acc ++ ParSeq(k), Math.sqrt(n / k).toInt))
            case (n, k, acc, sqr) if (k < sqr)  => factors(n, k + 1, acc, sqr)
            case (n, k, acc, sqr) if (k >= sqr) => factors((1, k, acc ++ ParSeq(n), 0))
        }
    }
    factors((n, 2, List[Long](), Math.sqrt(n).toInt))
}
def properDivisors(n: Long) = {
    val factors = factorize(n)
    val products = (1 until factors.length).map(i =>
        factors.combinations(i).map(_.product).toList).flatten
    (1L +: products).filter(_ < n)
}

def format(i: Long, divisors: Seq[Long]) = f"$i%5d    ${divisors.length}%2d   ${divisors mkString " "}"

println(f"    n   cnt   PROPER DIVISORS")
val (count, list) = (1L to 20000L).foldLeft( (0L, List[Long]()) ) { (max, i) =>
    val divisors = properDivisors(i)
    if (i <= 10L || i == 100L) println( format(i, divisors) )
    if (max._1 < divisors.length) (divisors.length, List(i))
    else if (max._1 == divisors.length) (divisors.length, max._2 ::: List(i))
    else max
}

list.foreach( number => println(f"$number%5d    ${properDivisors(number).length}") )
// vim: set ts=4 sw=4 et:
