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

def createAliquotSeq(n: Long, step: Int, list: List[Long]): (String, List[Long]) = {
    val sum = properDivisors(n).sum
    if (sum == 0) ("terminate", list ::: List(sum))
    else if (step >= 16 || sum > 140737488355328L) ("non-term", list)
    else {
        list.indexOf(sum) match {
            case -1 => createAliquotSeq(sum, step + 1, list ::: List(sum))
            case 0 => if (step == 0) ("perfect", list ::: List(sum))
                else if (step == 1) ("amicable", list ::: List(sum))
                else ("sociable-" + (step + 1), list ::: List(sum))
            case index => if (step == index) ("aspiring", list ::: List(sum))
                else ("cyclic-" + (step - index + 1), list ::: List(sum))
        }
    }
}
val numbers = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 28, 496, 220, 1184,
    12496, 1264460, 790, 909, 562, 1064, 1488, 15355717786080L)
val result = numbers.map(i => createAliquotSeq(i, 0, List(i)))

result foreach { v => println(f"${v._2.head}%14d ${v._1}%10s [${v._2 mkString " "}]" ) }
// vim: set ts=4 sw=4 et:
