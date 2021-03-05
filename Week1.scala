object Week1 {
    // Simple parallel sorting algorithm
    def sort(from: Int, until: Int, depth: Int): Unit = {
        if ( depth == maxDepth )
            quicksort(xs, from, until - from )
        else {
            val mid = (from + until) / 2
            parallel(sort(mid, until, depth+1), sort(from,mid,depth+1))

            // Merge TODO
        } 
    }




    
    def main(args: Array[String]) = {
        println("Hello, world")
    }
}
