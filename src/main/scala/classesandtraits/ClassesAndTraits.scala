package classesandstraits

// Homework
//
// Add additional 2D shapes such as triangle and square.
//
// In addition to the 2D shapes classes, add also 3D shapes classes
// (origin, point, sphere, cube, cuboid, 3D triangle - you can add
// others if you think they are a good fit).
//
// Add method `area` to 2D shapes.
//
// Add methods `surfaceArea` and `volume` to 3D shapes.
//
// If some of the implementation involves advanced math, it is OK
// to skip it (leave unimplemented), the primary intent of this
// exercise is modelling using case classes and traits, and not math.

object ClassesAndTraits {
    class MutablePoint(var x: Double, var y: Double) {
        def move(dx: Double, dy: Double): Unit = {
        x = x + dx
        y = y + dy
        }
        override def toString: String =
        s"($x, $y)"
    }

    sealed trait Shape2D extends Located2D with Bounded

    sealed trait Shape3D extends Shape with Located3D with Bounded3D

    sealed trait Located2D {
        def x: Double
        def y: Double
    }

    sealed trait Located3D extends Located2D {
        def z: Double
    }

    sealed trait Bounded {
        def minX: Double
        def maxX: Double
        def minY: Double
        def maxY: Double
    }

    sealed trait Bounded3D extends Bounded {
        def minZ: Double
        def maxZ: Double
    }

    final case class Point(x: Double, y: Double) extends Shape2D {
        override def minX: Double = x
        override def maxX: Double = x
        override def minY: Double = y
        override def maxY: Double = y
    }

    final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape2D {
        override def x: Double = centerX
        override def y: Double = centerY
        override def minX: Double = x - radius
        override def maxX: Double = x + radius
        override def minY: Double = y - radius
        override def maxY: Double = y + radius
    }

    final case class Rectangle(leftX: Double, bottomY: Double, length: Double, height: Double) extends Shape2D {
        override def minX: Double = leftX
        override def maxX: Double = leftX + length
        override def minY: Double = bottomY
        override def maxY: Double = bottomY + height
        override def x: Double = leftX
        override def y: Double = bottomY
    }

    final case class Triangle(point1: Point, point2: Point, point3: Point) extends Shape2D {
        override def minX: Double = Set(point1, point2, point3).map(_.minX).min
        override def maxX: Double = Set(point1, point2, point3).map(_.maxX).max
        override def minY: Double = Set(point1, point2, point3).map(_.minY).min
        override def maxY: Double = Set(point1, point2, point3).map(_.maxY).max
        override def x: Double = minX
        override def y: Double = minY
    }
    
    final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D {
        override def minX: Double = x
        override def maxX: Double = x
        override def minY: Double = y
        override def maxY: Double = y
        override def minZ: Double = z
        override def maxZ: Double = z
    }

    final case class Square(startingPoint: Point3D, width: Double) extends Shape2D {
        override val x = startingPoint.x
        override val y = startingPoint.y
        override val z = startingPoint.z

        override def minX: Double = x
        override def maxX: Double = x + width
        override def minY: Double = y
        override def maxY: Double = y + width
        override def minZ: Double = z
        override def maxZ: Double = z
    }

    final case class Cube(x: Double, y: Double, z: Double, edge: Double) extends Shape3D {
        override def minX: Double = x
        override def maxX: Double = x + edge
        override def minY: Double = y
        override def maxY: Double = y + edge
        override def minZ: Double = z
        override def maxZ: Double = z + edge
    }

    final case class Sphere(centerX: Double, centerY: Double, centerZ: Double, radius: Double) extends Shape3D {
        override def x: Double = centerX
        override def y: Double = centerY
        override def z: Double = centerZ
        override def minX: Double = centerX - radius
        override def maxX: Double = centerX + radius
        override def minY: Double = centerY - radius
        override def maxY: Double = centerY + radius
        override def minZ: Double = centerZ - radius
        override def maxZ: Double = centerZ + radius
    }

    def minimumBoundingRectangle(objects: Set[Bounded]): Bounded = {
        new Bounded {
        implicit private val doubleOrdering: Ordering[Double] = Ordering.Double.IeeeOrdering
        override def minX: Double = objects.map(_.minX).min
        override def maxX: Double = objects.map(_.maxX).max
        override def minY: Double = objects.map(_.minY).min
        override def maxY: Double = objects.map(_.maxY).max
        }
    }

    def describe(x: Shape): String = x match {
        case Point(x, y) => s"Point(x = $x, y = $y)"
        case Circle(centerX, centerY, radius) => s"Circle(centerX = $centerX, centerY = $centerY, radius = $radius)"
        case Rectangle(leftX, bottomY, lenght, height) => s"Rectangle(leftX = $leftX, bottomY = $bottomY, lenght = $lenght"
        s", height = $height"
        case Triangle(point1, point2, point3) => s"Triangle(" + describe(point1) + ", " + describe(point2) + ", "  +
        describe(point3) + " )"
        case Point3D(x, y, z) => s"Point(x = $x, y = $y, z = $z)"
    }

    def area(x: Shape): Double = x match {
        case Point(x, y) => 0
        case Circle(centerX, centerY, radius) => Math.PI * radius * radius
        case Rectangle(leftX, bottomY, length, height) => length * height
        case Square(leftX, bottomY, length) => length * length
        case Triangle(point1, point2, point3) =>
        Math.abs((point1.x * (point2.y - point3.y) + point2.x * (point3.y - point1.y) +
            point3.x * (point1.y - point2.y)) / 2)
    }

    def surfaceArea(x: Shape): Double = x match {
        case Point3D(x, y, z) => 0
        case Cube(x, y, z, edge) =>  6 * edge * edge
        case Square(centerX, centerY, centerZ, radius) => 4 * math.Pi * radius * radius
    }

    def volume(x: Shape): Double = x match {
        case Point3D(x, y, z) => 0
        case Cube(x, y, z, edge) => math.pow(edge, 3)
        case Sphere(centerX, centerY, centerZ, radius) => (4 * math.Pi * math.pow(radius, 3)) / 3
    }

    object Origin extends Located2D {
        override def x: Double = 0
        override def y: Double = 0
    }

    object Origin3D extends Located3D {
        override def x: Double = 0
        override def y: Double = 0
        override def z: Double = 0
    }

    object Bounded {
        def minimumBoundingRectangle(objects: Set[Bounded]): Bounded =
        ClassesAndTraits.minimumBoundingRectangle(objects)
    }
}