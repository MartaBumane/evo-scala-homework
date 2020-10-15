package implicits

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

object Implicits {
    object SuperVipCollections4s {
        type SizeScore = Int

        trait GetSizeScore[T] {
            def apply(value: T): SizeScore
        }

        object syntax {
            implicit class GetSizeScoreOps[T: GetSizeScore](inner: T)(implicit getSizeScore: GetSizeScore[T]){
                def sizeScore: SizeScore = getSizeScore.apply(inner)
            }
        }

        final class MutableBoundedCache[K: GetSizeScore, V: GetSizeScore](maxSizeScore: SizeScore) {
            import syntax._
            import instances._

            private val map = mutable.LinkedHashMap.empty[K, V]

            def put(key: K, value: V): Unit = {
                val sumOfSizeScores = key.sizeScore + value.sizeScore

                while (!map.isEmpty && (size + sumOfSizeScores) > maxSizeScore) {
                    map.remove(map.head._1)
                }

                map += ((key, value))
            }

            def get(key: K): Option[V] = Try(map(key)).toOption
        }

        final case class PackedMultiMap[K, +V](inner: ArraySeq[(K, V)])
        object PackedMultiMap {
            def empty[K, V]: PackedMultiMap[K, V] = PackedMultiMap()
            def apply[K, V](values: (K, V)*): PackedMultiMap[K, V] =
              PackedMultiMap(inner = ArraySeq(values: _*))
        }

        trait Iterate[-F[_]] {
            def iterator[T](f: F[T]): Iterator[T]
        }

        trait Iterate2[-F[_, _]] {
            def iterator1[T, S](f: F[T, S]): Iterator[T]
            def iterator2[T, S](f: F[T, S]): Iterator[S]
        }

        object instances {
            import syntax._

            implicit val iterableOnceIterate: Iterate[Iterable] = new Iterate[Iterable] {
                override def iterator[T](f: Iterable[T]): Iterator[T] = f.iterator
            }

            implicit val arrayIterate: Iterate[Array] = new Iterate[Array] {
                override def iterator[T](f: Array[T]): Iterator[T] = f.iterator
            }

            implicit def mapIterate: Iterate2[Map] = new Iterate2[Map] {
                def iterator1[A, B](f: Map[A, B]): Iterator[A] = f.keys.iterator
                def iterator2[A, B](f: Map[A, B]): Iterator[B] = f.values.iterator
            }

            implicit def packedMultiMapIterate2: Iterate2[PackedMultiMap] = new Iterate2[PackedMultiMap] {
                def iterator1[A, B](f: PackedMultiMap[A, B]): Iterator[A] = f.inner.toMap.keys.iterator
                def iterator2[A, B](f: PackedMultiMap[A, B]): Iterator[B] = f.inner.toMap.values.iterator
            }

            implicit def byteGetSizeScore: GetSizeScore[Byte] = (_: Byte) => 1
            implicit def charGetSizeScore: GetSizeScore[Char] = (_: Char) => 2
            implicit def intGetSizeScore: GetSizeScore[Int]   = (_: Int) => 4
            implicit def longGetSizeScore: GetSizeScore[Long] = (_: Long) => 8
            implicit def stringGetSizeScore: GetSizeScore[String] = (s: String) => 12 + s.length * 2
            implicit def arrayGetSizeScore[T: GetSizeScore]: GetSizeScore[Array[T]] = a => 12 + a.map(_.sizeScore).sum
            implicit def listGetSizeScore[T: GetSizeScore]: GetSizeScore[List[T]] = l => 12 + l.map(_.sizeScore).sum
            implicit def vectorGetSizeScore[T: GetSizeScore] : GetSizeScore[Vector[T]] = v => 12 + v.map(_.sizeScore).sum
            implicit def mapGetSizeScore[T: GetSizeScore]: GetSizeScore[Map[T, T]] = (x: Map[T, T]) => {
                if (x.isEmpty){
                    12
                } else {
                    12 + x.keys.map(sizeScore(_)).sum + x.values.map(sizeScore(_)).sum
                }
            }
            implicit def packedMultiMapGetSizeScore[T: GetSizeScore]: GetSizeScore[PackedMultiMap[T, T]] = (value: PackedMultiMap[T, T]) => {
                if (value.inner.isEmpty) {
                    12
                } else {
                    value.inner.map {
                        case (x, y) => x.sizeScore + y.sizeScore
                    }.sum + 12
                }
            }
        }
    }

    object MyTwitter {
        import SuperVipCollections4s._

        final case class Twit(
            id: Long,
            userId: Int,
            hashTags: Vector[String],
            attributes: PackedMultiMap[String, String],
            fbiNotes: List[FbiNote]
        )

        final case class FbiNote(
            month: String,
            favouriteChar: Char,
            watchedPewDiePieTimes: Long
        )

        trait TwitCache {
            def put(twit: Twit): Long
            def get(id: Long): Option[Twit]
        }

        def createTwitCache(maxSizeScore: SizeScore): TwitCache = {
            val mutableCache = new MutableBoundedCache[Long, Twit](maxSizeScore)

            override def put(twit: Twit): Unit = mutableCache.put(twit.id, twit)

            override def get(id: Long): Option[Twit] = mutableCache.get(id)
        }
    }
}