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

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
        case Cons(head, tail) if f(head) => dropWhile(tail,f)
        case _ => l
    }

    /*
     Tracing the call stack
     append(List(1,2,3),List(4,5,6))
     Cons(1,append(List(2,3),(4,5,6))
     Cons(2,append(List(3), (4,5,6)))
     Cons(3,append(List(Nil), (4,5,6) ))
     Cons(3,4,5,6))
     Cons(2,3,4,5,6)
     Cons(1,2,3,4,5,6)
    */
    def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
        case Nil => a2
        case Cons(head, tail) => Cons(head, append(tail, a2))
    }

    def init[A](l: List[A]): List[A] = l match {
        case Nil => Nil
        case Cons(_,Nil) => Nil
        case Cons(head,tail) => Cons(head,init(tail))
    }

    def dropWhileCurried[A](l : List[A])(f : A => Boolean) : List[A] = l match {
        case Cons(head, tail) if f(head) => dropWhileCurried(tail)(f)
        case _ => l     
    }  

    /*
        Usage
        foldRight(List(1,2,3,4,5),0)((x,y) => x + y) or foldRight(List(1,2,3,4,5),0)(_ + _) 
        foldRight(List(1,2,3,4,5),1)((x,y) => x * y) or foldRight(List(1,2,3,4,5),1)(_ * _)
    */
    def foldRight[A,B](l : List[A], d : B)(f : (A, B) => B) : B = l match {
        case Nil => d
        case Cons(head,tail) =>  f(head,foldRight(tail,d)(f)) 
    }

     @annotation.tailrec
    def foldLeft[A,B](l: List[A], d: B)(f: (B, A) => B): B = l match {
        case Nil => d
        case Cons(head,tail) => foldLeft(tail,f(d,head))(f) 
    }

    def length[A](l : List[A]) : Int = {
        foldRight(l,0)((_,total) => 1 + total)
    }
 
    // the magic happens here in the variadic function
    def apply[A](as: A*): List[A] = {
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
    }

}
