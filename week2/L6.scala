object L6 {

    // Sequential scan left
    def scanLeft[A](inp: Array[A], a0: A, f: (A,A) => A, out: Array[A]): Unit = {
        out(0) = a0
        var a = a0
        var i = 0
        while ( i < inp.length ) {
            a = f(a, inp(i))
            i += 1
            out(i) = a
        }
    }

    // declarations of reduceseg and mapseg
    def reduceSeg1[A](inp: Array[A], left: Int, right: Int, a0: A, f: (A,A) => A ): A = ???
    def mapSeg[A,B](inp: Array[A], left: Int, right: Int, fi: (Int,A) => B, out: Array[B]): Unit = ???

    // Parallel scan left
    def scanLeft[A](inp: Array[A], a0: A, f: (A,A) => A, out: Array[A]): Unit = {
        val fi = { (i: Int, v: A) => reduceSeg1(inp,0,i,a0,f) }
        mapSeg(inp,0,inp.length,fi,out)
        val last = inp.length - 1
        out(last+1) = f(out(last),inp(last))
    }

    // Tree definition
    sealed abstract class Tree[A]
    case class Leaf[A](v: A) extends Tree[A]
    case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]

    // Tree definition
    sealed abstract class TreeRes[A]( val res: A )
    case class LeafRes[A](override val res: A) extends TreeRes[A]
    case class NodeRes[A](l: TreeRes[A], override val res: A, r: TreeRes[A]) extends TreeRes[A]

    // parallel scan left for tree

    // Reduce that transforms tree into tree res
    def upsweep[A](t: Tree[A], f: (A,A) => A): TreeRes[A] = t match {
        case Leaf(v) => LeafRes(v)
        case Node(l, r) => {
            val( lb, rb ) = parallel(upsweep(l, f), upsweep(r, f) )
            NodeRes(lb, f(lb.res,rb.res), rb)
        }
    }

    // downsweep
    def downsweep[A](t: TreeRes[A], a0: Int, f: (A,A) => A): Tree[A] = t match {
        case LeafRes(a) => Leaf(f(a0,a))
        case NodeRes(l, a, r) => {
            val (lb,rb) = parallel( downsweep(l,a0,f), downsweep(r,f(a0,l.res),f) )
            Node(lb,rb) 
        } 
    }

    // prepend
    def prepend[A](a0: Int, t: Tree[A]): Tree[A] = t match {
        case Leaf(v) => Node(Leaf(a0),Leaf(v))
        case Node(l, r) => Node(prepend(a0,l), r)
    }

    // scan left
    def scanLeft[A](t: Tree[A], a0: Int, f: (A,A) => A): Tree[A] = {
        val tRes = upsweep(t,f)
        val scan1 = downsweep(tRes,a0,f)
        prepend(a0,scan1)
    }

    // New tree definitions
    sealed abstract class TreeResA[A]( val res: A )
    case class Leaf[A](from: Int, to: Int, override val res: A) extends TreeResA[A]
    case class Node[A](l: TreeResA[A], override val res: A, r: TreeResA[A]) extends TreeResA[A]

    // Reduce that transforms tree into tree res
    def upsweep[A](inp: Array[A], from: Int, to: Int, f: (A,A) => A): TreeResA[A] = {
        if ( to - from < threshold )
            Leaf(from,to,reduceSeg1(inp, from + 1, to, inp(from), f))
        else {
            val mid = from + ( to - from ) / 2 
            val (lb,rb) = parallel(upsweep(inp,from,mid,f),upsweep(inp,mid,to,f))
            Node(lb,f(lb.res,rb.res),rb)
        }
    }

    def reduceSeg1[A](inp: Array[A], from: Int, to: Int, a0: A, f: (A,A) => A ): A = {
        val res = a0
        val i = from
        while ( i < to ) {
            res = f(res,inp(i))
            i += 1 
        }
        res
    }

    def downsweep[A](inp: Array[A])
}