def fibntr(n: Int): Int = {
    if (n < 2) n
    else fib(n-1) + fib(n-2)
}

def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, current: Int, acc0: Int, acc1: Int): Int = {
        if (current == n) acc1
        else loop(n, current + 1, acc1, acc0 + acc1)
    }
    if (n <= 1) n
    else loop(n, 1, 0, 1)
} 

println(fib(0))
println(fib(1))
println(fib(2))
println(fib(3))
println(fib(4))
println(fib(5))
println(fib(6))
println(fib(10) == fibntr(10))
