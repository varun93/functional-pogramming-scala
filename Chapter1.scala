object MyModule{
    def abs(n : Int) : Int = {
        if(n < 0) -n
        else n
    }
    
    private def formatAbs(value : Int) = {
        val message = "The absolute value of %d is %d"
        message.format(value,abs(value)) 
    }

    def fibonacci(n : Int) : Int = {
        @annotation.tailrec
        def loop(n : Int, a : Int, b : Int) : Int = {
            if(n < 3) b
            else loop(n-1, b, a+b)
        }
        loop(n,0,1)
    }

    def factorial(n : Int) : Int = {
        @annotation.tailrec
        def loop(n : Int, acc : Int) : Int = {
            if(n <= 0) acc
            else loop(n - 1,acc*n)
        }
        loop(n, 1)
    }
    
    def main(args : Array[String]) : Unit  =
    println(formatAbs(-42))
}