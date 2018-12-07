def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a: A => b: B => f(a, b)
}

def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
}

def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
}


//Tests
def square(x: Int): Int = {
    x * x
}

def incr(x: Int): Int = {
    x + 1
}

def sum(x: Int, y: Int): Int = {
    x + y
}

println(curry(sum)(1)(9) == uncurry(curry(sum))(1, 9))
println(compose(incr, square)(9))
