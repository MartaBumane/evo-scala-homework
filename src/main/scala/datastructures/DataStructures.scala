package datastructures

object DataStructures {
    def sortConsideringEqualValues[T](map: Map[T, Int]): List[(Set[T], Int)] ={
        map.map{
            case (_, v) => (map.filter({case (_, b) => v == b}).map(_._1).toSet,v)
        }.toList.sortBy({case (_, y) => y});
    }
}
