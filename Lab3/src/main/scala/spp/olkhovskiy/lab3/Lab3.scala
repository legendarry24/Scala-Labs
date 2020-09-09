package spp.olkhovskiy.lab3

import scala.reflect.ClassTag

object Lab3 {
	def main(args: Array[String]): Unit = {
		// Task 1
		val pair = (1, 2)
		println(s"Task 1: Initial pair: $pair. Reversed pair: ${swap(pair)}")

		// Task 2
		val array = Array(1, 2, 3, 4)
		println(s"Task 2: Initial array: ${array.mkString(", ")}. " +
				s"After swapping first 2 items: ${swap(array).mkString(", ")}")

		// Task 3
		val list = List(List(3, 8), 2, List(5))
		println(s"Task 3: Initial tree: $list. Leaves sum: ${leafSum(list)}")

		// Task 4
		val tree: Node =
			Node(
				Leaf(1),
				Node(Node(Leaf(2),
					Node(Leaf(3),
						Leaf(4))),
					Node(Leaf(5),
						Leaf(6))))
		println(s"Task 4.1: Initial tree: $tree. Leaves sum: ${treeLeafSum(tree)}")

		// Task 4.2
		val tree2: PolymorphNode =
			PolymorphNode(
				PolymorphLeaf(1),
				PolymorphNode(
					PolymorphNode(
						PolymorphLeaf(2),
						PolymorphNode(
							PolymorphLeaf(3),
							PolymorphLeaf(4))),
					PolymorphNode(
						PolymorphLeaf(5),
						PolymorphLeaf(6))))
		println(s"Task 4.2: Initial tree: $tree2. Leaves sum: ${polymorphTreeLeavesSum(tree2)}")

		// Task 5
		val tree3: PolyNode =
			PolyNode(
				PolyLeaf(1),
				PolyNode(
					PolyNode(
						PolyLeaf(2),
						PolyLeaf(3),
						PolyLeaf(4)),
					PolyNode(
						PolyLeaf(5),
						PolyLeaf(6))))
		println(s"Task 5: Initial tree: $tree3. Leaves sum: ${polyTreeSum(tree3)}")

		// Task 6
		val tree4 =
			PolyTreeWithEval('+',
				PolyLeaf(7),
				PolyTreeWithEval('-',
					PolyLeaf(5),
					PolyLeaf(3)),
				PolyTreeWithEval('*',
					PolyLeaf(2),
					PolyLeaf(4),
					PolyTreeWithEval('/',
						PolyLeaf(4),
						PolyLeaf(2))))

		println(s"Task 6: Initial tree: $tree4; Leaves eval: ${eval(tree4)}")

		// Task 7
		val pair1 = new ImmutablePair[Int, String](5, "five")
		println(s"Task 7: Initial pair: $pair1. Pair after swapping: ${pair1.swap()}")

		// Task 8
		val pair2 = new MutablePair[Int](3, 4)
		println(s"Task 8: Initial pair: $pair2. Pair after swapping: ${pair2.swap()}")

		// Task 9
		val pair4 = new ImmutablePair[Int, String](7, "seven")
		val pair5 = new ImmutablePair[Int, String](5, "five")
		println(s"Task 9: Initial pair: $pair4. Pair after swapping: ${pair4.swap(pair5)}")

		// Task 10
		val arr = Array(1,2,3,4,5)
		println(s"Task 10: Initial array: ${arr.mkString(",")}. " +
				s"Array's middle element: ${middle(arr).orNull}")

		// Task 11
		val pair6 = new Pair[String, String]("left", "right")
		val pair7 = new Pair[String, String]("left", "right")
		pair7.swap
		println(s"Task 11: Initial pair: $pair6. Pair after swapping: $pair7")
	}

	// Task 1
	def swap(pair: (Int, Int)): (Int, Int) = pair match {
		case pair => pair.swap
	}

	// Task 2
	def swap[T: ClassTag](array: Array[T]): Array[T] = array match {
		case Array(first, second, tail @ _*) => Array(second, first) ++ tail
		case _ => array
	}

	// Task 3
	def leafSum(leaves: List[Any]): Int = leaves.foldLeft(0) {
		case (sum: Int, node: List[_]) => sum + leafSum(node)
		case (sum: Int, leaf: Int) => sum + leaf
		case (sum: Int, _) => sum
	}

	// Task 4
	sealed abstract class BinaryTree

	case class Leaf(value: Int) extends BinaryTree

	case class Node(left: BinaryTree, right: BinaryTree) extends BinaryTree

	// Task 4.1
	def treeLeafSum(root: BinaryTree): Int = root match {
		case Node(left, right) => treeLeafSum(left) + treeLeafSum(right)
		case Leaf(value) => value
	}

	// Task 4.2
	sealed abstract class PolymorphBinaryTree(val sum: Int)

	case class PolymorphLeaf(value: Int) extends PolymorphBinaryTree(value)

	case class PolymorphNode(left: PolymorphBinaryTree, right: PolymorphBinaryTree) extends PolymorphBinaryTree(left.sum + right.sum)

	def polymorphTreeLeavesSum(root: PolymorphBinaryTree): Int = {
		root.sum
	}

	// Task 5
	sealed abstract class PolyTree

	case class PolyLeaf(value: Int) extends PolyTree

	case class PolyNode(childNodes: PolyTree*) extends PolyTree

	def polyTreeSum(root: PolyTree): Int = root match {
		case PolyNode(childNodes @ _*) => childNodes.map(polyTreeSum).sum
		case PolyLeaf(value) => value
	}

	// Task 6
	case class PolyTreeWithEval(operator: Char, nodes: PolyTree*) extends PolyTree

	def eval(tree: PolyTree): Int = tree match {
		case PolyLeaf(value) => value
		case PolyTreeWithEval('+', nodes @_*) => nodes.map(eval).sum
		case PolyTreeWithEval('-', nodes @_*) => -nodes.map(eval).sum
		case PolyTreeWithEval('*', nodes @_*) => nodes.map(eval).product
		case PolyTreeWithEval('/', nodes @_*) => nodes.map(eval).reduceLeft((acc, curr) => acc / curr)
	}

	// Task 7
	class ImmutablePair[T, S](val left: T, val right: S) {
		def swap(): ImmutablePair[S, T] = new ImmutablePair(right, left)

		// Task 9
		def swap[TLeft, TRight](pair: ImmutablePair[TLeft, TRight]): ImmutablePair[TRight, TLeft] = {
			new ImmutablePair(pair.right, pair.left)
		}

		override def toString: String = s"($left,$right)"
	}

	// Task 8
	class MutablePair[T <: Int](var left: T, var right: T) {
		def swap(): MutablePair[T] = {
			val temp = left
			left = right
			right = temp
			this
		}

		override def toString: String = s"($left,$right)"
	}

	// Task 10
	def middle[C, V](seq: C)(implicit iterable: C => Iterable[V]): Option[V] = {
		val size = seq.size
		if (size % 2 == 0) {
			return None
		}

		var distance = size / 2
		seq.find { _ =>
			val found = distance == 0
			distance -= 1
			found
		}
	}

	// Task 11
	class Pair[S, T](var left: S, var right: T) {
		def swap(implicit equal1: T =:= S, equal2: S =:= T): Unit = {
			val temp = left
			left = right
			right = temp
		}

		override def toString: String = s"($left,$right)"
	}
}