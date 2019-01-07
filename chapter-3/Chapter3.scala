sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head : A, tail : List[A]) extends List[A] 

object List{
    def sum(ints : List[Int]) : Int = ints match {
        case Nil => 0
        case Cons(head,tail) => head + sum(tail)
    }

    def product(p : List[Double]) : Double = p match {
        case Nil => 1.0
        case Cons(0,_) => 0.0
        case Cons(head,tail) => head * product(tail) 
    }

    def tail[A](list : List[A]) : List[A] = list match {
        case Nil => Nil
        case Cons(_,tail) => tail
    }

    def setHead[A](list : List[A],newHead : A) : List[A] = list match  {
        case Nil => Cons(newHead,Nil)
        case Cons(_,tail) => Cons(newHead,tail)
    }

    def drop[A](l: List[A], n: Int): List[A] = l match {
        case Nil => Nil
        case Cons(_,tail) => if(n <= 0) {
            l
        } 
        else{
            drop(tail, n - 1)   
        }
    }

    // the magic happens here
    def apply[A](as: A*): List[A] = {
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
    }

}
