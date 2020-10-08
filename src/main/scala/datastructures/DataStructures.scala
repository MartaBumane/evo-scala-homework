package datastructures

object DataStructures {
    def sortConsideringEqualValues[T](map: Map[T, Int]): List[(Set[T], Int)] = {
        val groupedByValue = map.groupBy { case (_, value) => value }
        val mapSorted = groupedByValue
            .toList
            .map {
                case (key, values) => (values.keySet, key)
            }
            .sortBy {
                case (_, value) => value
            }
            
        mapSorted;
    }
}
