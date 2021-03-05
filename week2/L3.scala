object L3 {
    // Tree definition
    sealed abstract class Tree[A]
    case class Leaf[A](v: A) extends Tree[A]
    case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]

    // Parallel reduce on tree
    def reduce[A](t: Tree[A], f: (A,A) => A ): A = t match {
        case Leaf(v) => v
        case Node(l, r) => {
            val (lb, rb) = parallel( reduce[A](l,f), reduce[A](r,f) )
            f(lb,rb)
        }
    }

    // to list
    def toList[A](t: Tree[A]): List[A] = t match {
        case Leaf(v) => List(v)
        case Node(l, r) => toList[A](l) ++ toList[A](r)
    }

    // map
    def map[A,B](t: Tree[A], f: A => B): Tree[B] = t match {
        case Leaf(v) => Leaf(f(v))
        case Node(l, r) => Node(map[A,B](l,f), map[A,B](r,f))
    }

    // to list 2
    def toList2[A](t: Tree[A]): List[A] = {
        reduce(map(t,List(_)), _ ++ _ ) 
    }

    // parallel array reduce
    def reduceSeg[A](inp: Array[A], left: Int, right: Int, f: (A,A) => A ): A = {
        if ( right - left < threshold ) {
            var res = inp(left); var i = left + 1
            while ( i < right ) {
                res = f(res, inp(i))
                i += 1
            }
            res
        }
        else {
            val mid = left + ( right - left ) / 2
            val (lV, rV) = parallel(reduceSeg(inp,left,mid,f), reduceSeg(inp,mid,right,f))
            f(lV,rV)
        }
    }
    def reduce[A](inp: Array[A], f: (A,A) => A ): A = {
        reduceSeg(inp, 0, inp.length, f)
    }

    
}