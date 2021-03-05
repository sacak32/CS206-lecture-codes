object L1_2 {
    // Simple parallel sorting algorithm
    def sort(from: Int, until: Int, depth: Int): Unit = {
        if ( depth == maxDepth )
            quicksort(xs, from, until - from )
        else {
            val mid = (from + until) / 2
            parallel(sort(mid, until, depth+1), sort(from,mid,depth+1))

            // Merge 
            val flip = (maxDepth - depth) % 2 == 0 
            val src = if (flip) ys else xs
            val dst = if (flip) xs else ys
            merge(src, dst, from, mid, until)
        } 
    }
    sort(0, xs.length, 0)

    // Simple parallel copy algorithm
    def copy(src: Array[Int], target: Array[Int], from: Int, until: Int, depth: Int ): Unit = {
        if ( depth == maxDepth )
            Array.copy(src,from,target,from,until-from)
        else {
            val mid = (from + until) / 2
            parallel( copy(src,target,from,mid,depth+1), copy(src,target,mid,until,depth+1) )
        }
    }
    if (maxDepth % 2 == 0) copy(ys,xs,0,xs.length,0)
    
    // Sequential map on lists
    def mapSeq[A,B](lst: List[A], f: A => B): List[B] = lst match {
        case Nil => Nil
        case h :: t => f(h) :: mapSeq(t,f)
    }

    // Sequential map on arrays
    def mapASeqSeg[A,B](inp: Array[A], left: Int, right: Int, f: A => B, out: Array[B]): Unit = {
        var i = left
        while ( i < right ) {
            out(i) = f(inp(i))
            i += 1
        }
    }

    // Parallel map on arrays
    def mapASeqPar[A,B](inp: Array[A], left: Int, right: Int, f: A => B, out: Array[B]): Unit = {
        if (right - left < threshold) 
            mapASeqSeg(inp,left,right,f,out)
        else {
            val mid = (left + right) / 2
            parallel(mapASeqPar(inp,left,mid,f,out),mapASeqPar(inp,mid,right,f,out)) 
        }
    }

    // Tree definition
    sealed abstract class Tree[A] { val size: Int }
    case class Leaf[A](a: Array[A]) extends Tree[A] {
        override val size = a.size
    } 
    case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A] {
        override val size = l.size + r.size
    }

    // Tree parallel map
    def mapTreePar[A: Manifest, B: Manifest](t: Tree[A], f: A => B): Tree[B] = t match {
        case Leaf(a) => {
            val len = a.length; val b = new Array[B](len)
            val i = 0
            while ( i < len ) {
                b(i) = f(a(i))
                i += 1
            }
            Leaf(b)
        }
        case Node(l, r) => {
            val (ls, rs) = parallel(mapTreePar(l,f), mapTreePar(r,f))
            Node(ls,rs)
        }
    }

    // Reduce fold test
    List(1,3,8).foldRight(100)( (s,x) => s - x ) == 1 - ( 3 - ( 8 - 100 ) )
    List(1,3,8).foldLeft(100)( (s,x) => s - x ) == ( ( 100 - 1 ) - 3 ) - 8 
    List(1,3,8).reduceRight( (s,x) => s - x ) == 1 - ( 3 - 8 )
    List(1,3,8).reduceLeft( (s,x) => s - x ) == ( 1 - 3 ) - 8 
}
