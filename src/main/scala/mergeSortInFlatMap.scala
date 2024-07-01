class Sort{

    def mergeSort(ls:List[Int]): List[Int] = {

        ls
    }
    def Merge(a:List[Int],b:List[Int],acc:List[Int]): List[Int] = (a,b) match {
        case (Nil,Nil) => acc
        case (x,Nil) => acc ++ x
        case (Nil,x) => acc ++ x 
        case (x::rest,y::rest2) if x > y => Merge(rest,b,acc:+x)
        case (x::rest,y::rest2) if x < y => Merge(a,rest2,acc:+y)
    }
}