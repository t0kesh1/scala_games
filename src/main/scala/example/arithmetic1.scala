package arithmetic {

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
    implicit class S99Int(val start: Int) {
        def isPrime: Boolean = ( start > 1 ) && (primes takeWhile { _ <= Math.sqrt(start)} forall {start % _ != 0})
        def isCoPrimeTo(i: Int): Boolean = gcd(start,i) == 1
        def totient: Int = List.range(1,start).count(this.isCoPrimeTo(_))
        def gcd(m:Int,n:Int): Int = (m,n) match {
            case (x,0) => x
            case (x,y) => gcd(y, x % y) }
    }

    val primes = Stream.cons(2, Stream.from(3,2) filter {S99Int(_).isPrime} )

  }
}