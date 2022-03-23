// Self-verifying. Output removed so it can
// successfully compare against empty file.
use io
use conv

gcd(a:int, b:int):int {
    println("gcd")

    while (a != 0) {
        println("gcd loop")
        
        if (a<b) b = b - a
        else a = a - b
    }
    return b
}

isprime(n:int):bool {
    println("isprime")

    i:int = 2
    while (i*i <= n) {
        println("isprime loop")

        if (gcd(i, n) != 1) {
            return false
        }
        i = i+1
    }
    return true
}

largestprime(max:int):int {
    println("largestprime" + "here")

    a:int = 1
    largest:int = 1
    while (a < max) {
      println("largestprime loop")

      if (isprime(a)) largest = a
      a = a+1
    }
    return largest
}

main(args:int[][]) {
    println("main")

    print("Largest prime less than 1,000 is " + unparseInt(largestprime(10)))

    println("done")
}
