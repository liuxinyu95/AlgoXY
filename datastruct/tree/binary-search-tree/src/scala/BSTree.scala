/*
 * BSTree.scala
 * Copyright (C) 2018 Liu Xinyu (liuxinyu95@gmail.com)
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
import scala.util.Random                //for verification purpose
import scala.language.postfixOps

object BSTree {

  sealed trait Tree[+A]
  case object Empty extends Tree[Nothing]
  case class Node[A] (left: Tree[A], key: A, right: Tree[A]) extends Tree[A]

  // helpers
  def leaf[A] (x: A) : Tree[A] = Node(Empty, x, Empty)

  def isEmpty[A] (tr: Tree[A]): Boolean =
    tr match {
      case Empty => true
      case _ => false
    }

  // in-order traverse
  def map [A, B] (f: A => B, tr: Tree[A]): Tree[B] =
    tr match {
      case Empty => Empty
      case Node(left, x, right) => Node(map(f, left), f(x), map(f, right))
    }

  def lookup[A <% Ordered[A]] (tr: Tree[A], x: A): Tree[A] =
    tr match {
      case Empty => Empty
      case Node(left, y, right) =>
        if (x == y) tr
        else if (x < y) lookup(left, x)
        else lookup(right, x)
    }

  def min[A] (tr: Tree[A]): A =
    tr match {
      case Node(Empty, x, _) => x
      case Node(left, _, _) => min(left)
    }

  def max[A] (tr: Tree[A]): A =
    tr match {
      case Node(_, x, Empty) => x
      case Node(_, _, right) => max(right)
    }

  def insert[A <% Ordered[A]] (tr: Tree[A], x: A): Tree[A] =
    tr match {
      case Empty => Node(Empty, x, Empty)
      case Node(left, y, right) =>
        if (x < y) Node(insert(left, x), y, right)
        else Node(left, y, insert(right, x))
    }

  def delete[A <% Ordered[A]] (tr: Tree[A], x: A): Tree[A] =
    tr match {
      case Empty => Empty
      case Node(left, y, right) =>
        if (x < y) Node(delete(left, x), y, right)
        else if (y < x) Node(left, y, delete(right, x))
        else if (isEmpty(left)) right
        else if (isEmpty(right)) left
        else {
          val z = min(right)
          Node(left, z, delete(right, z))
        }
    }

  def fromList [A <% Ordered[A]] (xs: Seq[A]) : Tree[A] = ((Empty: Tree[A]) /: xs) (insert)

  def toList[A] (tr: Tree[A]) : List[A] =
    tr match {
      case Empty => List()
      case Node(left, x, right) => toList(left) ::: (x :: toList(right))
    }

  val N = 100
  def genList(r: Random) = r.shuffle(0 to N - 1).take(r.nextInt(N))

  def testBuild(xs: Seq[Int]) = {
    assert(toList(fromList(xs)) == xs.sortWith(_ < _), println("violate build invariant"))
  }

  def testLookup(xs: Seq[Int], x: Int) = {
    lookup(fromList(xs), x) match {
      case Empty => assert(!xs.contains(x), println(s"$x exists"))
      case Node(_, y, _) => assert(x == y, println(s"given $x, found $y"))
    }
  }

  def testMinMax(xs: Seq[Int]) = {
    if (!xs.isEmpty) {
      val tr = fromList(xs)
      assert(xs.min == min(tr), println(s"failed to find the min in $xs"))
      assert(xs.max == max(tr), println(s"failed to find the max in $xs"))
    }
  }

  def testDelete(xs: Seq[Int]) = {
    if (!xs.isEmpty) {
      xs.foldLeft((xs.sorted, fromList(xs))) {
        (t, x) => {
          val ys = t._1
          val tr = t._2
          assert(ys == toList(tr), println(s"inconsist delete result: $ys"))
          (ys diff List(x), delete(tr, x))
        }
      }
    }
  }

  def test() = {
    val r = Random
    for (_ <- 1 to N) {
      val xs = genList(r)
      testBuild(xs)
      testLookup(xs, r.nextInt(N))
      testMinMax(xs)
      testDelete(xs)
    }
    println(s"$N tests passed");
  }
}
