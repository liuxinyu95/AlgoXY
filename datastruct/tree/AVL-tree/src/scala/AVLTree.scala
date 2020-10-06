/*
 * AVLTree.scala
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
import scala.util.Random    //for verification
import scala.language.postfixOps
import scala.math

object AVLTree {
  sealed trait Tree[+A]
  case object Empty extends Tree[Nothing]
  case class Br[A] (left: Tree[A], key: A, right: Tree[A], delta: Int) extends Tree[A]

  def insert[A <% Ordered[A]] (tr: Tree[A], x: A): Tree[A] = ins(tr, x)._1

  // returns (t, d), where t: tree, d: delta of height
  def ins[A <% Ordered[A]] (tr: Tree[A], x: A): (Tree[A], Int) = tr match {
    case Empty => (Br(Empty, x, Empty, 0), 1)
    case Br(l, k, r, d) => if (x == k) (Br(l, k, r, d), 0)
      else if (x < k) tree(ins(l, x), k, (r, 0), d)
      else tree((l, 0), k, ins(r, x), d)
  }

  def delete[A <% Ordered[A]] (tr: Tree[A], x: A): Tree[A] = del(tr, x)._1

  // returns (t, d), where t: tree, d: decrement in height
  def del[A <% Ordered[A]] (tr: Tree[A], x: A): (Tree[A], Int) = tr match {
    case Empty => (Empty, 0)
    case Br(l, k, r, d) => if (x < k) tree(del(l, x), k, (r, 0), d)
      else if (x > k) tree((l, 0), k, del(r, x), d)
      else if (isEmpty(l)) (r, -1)
      else if (isEmpty(r)) (l, -1)
      else {
        val k1 = min(r)
        tree((l, 0), k1, del(r, k1), d)
      }
  }

  def isEmpty[A](tr: Tree[A]): Boolean = tr match {
    case Empty => true
    case _ => false
  }

  def min[A](tr: Tree[A]): A = tr match {
    case Br(Empty, x, _, _) => x
    case Br(left, x, _, _) => min(left)
  }

  def tree[A] (left: (Tree[A], Int), key: A, right: (Tree[A], Int), d: Int): (Tree[A], Int) = {
    val d1 = d + right._2 - left._2
    val delta = deltaH(d, d1, left._2, right._2)
    balance(Br(left._1, key, right._1, d1), delta)
  }

  /*
   * delta(Height) = max(|R'|, |L'|) - max (|R|, |L|)
   *  where we denote height(R) as |R|
   */
  def deltaH(d: Int, d1: Int, dl: Int, dr: Int): Int =
    if (d >= 0 && d1 >=0) dr
    else if (d <= 0 && d1 >= 0) d + dr
    else if (d >= 0 && d1 <= 0) dl - d
    else dl

  def balance[A] (tr: Tree[A], delta: Int): (Tree[A], Int) = (tr, delta) match {
    case (Br(Br(Br(a, x, b, dx), y, c, -1), z, d, -2), _) =>
      (Br(Br(a, x, b, dx), y, Br(c, z, d,  0), 0), delta - 1)
    case (Br(a, x, Br(b, y, Br(c, z, d, dz),  1),  2), _) =>
      (Br(Br(a, x, b,  0), y, Br(c, z, d, dz), 0), delta - 1)
    case (Br(Br(a, x, Br(b, y, c, dy),  1), z, d, -2), _) => {
      val dx1 = if (dy == 1) -1 else 0
      val dz1 = if (dy == -1) 1 else 0
      (Br(Br(a, x, b, dx1), y, Br(c, z, d, dz1), 0), delta - 1)
    }
    case (Br(a, x, Br(Br(b, y, c, dy), z, d, -1),  2), _) => {
      val dx1 = if (dy == 1) -1 else 0
      val dz1 = if (dy == -1) 1 else 0
      (Br(Br(a, x, b, dx1), y, Br(c, z, d, dz1), 0), delta - 1)
    }
    // delete specific fixing
    case (Br(Br(a, x, b, dx), y, c, -2), _) =>
      (Br(a, x, Br(b, y, c, -1), dx + 1), delta)
    case (Br(a, x, Br(b, y, c, dy), 2), _) =>
      (Br(Br(a, x, b, 1), y, c, dy - 1), delta)
    case _ => (tr, delta)
  }

  // verification
  def fromList [A <% Ordered[A]] (xs: Seq[A]) : Tree[A] =
    ((Empty: Tree[A]) /: xs) (insert)

  def toList[A] (tr: Tree[A]) : List[A] = tr match {
    case Empty => List()
    case Br(left, x, right, _) => toList(left) ::: (x :: toList(right))
  }

  def isAVL[A] (tr: Tree[A]): Boolean = tr match {
    case Empty => true
    case Br(left, _, right, d) => {
      val d = math.abs(height(right) - height(left))
      isAVL(left) && isAVL(right) && d <= 1
    }
  }

  def height[A] (tr: Tree[A]): Int = tr match {
    case Empty => 0
    case Br(left, _, right, _) => 1 + math.max(height(left), height(right))
  }

  def checkDelta[A] (tr: Tree[A]): Boolean = tr match {
    case Empty => true
    case Br(left, _, right, d) => checkDelta(left) && checkDelta(right) &&
      d == height(right) - height(left)
  }

  val N = 100
  def genList(r: Random) = r.shuffle(0 to N - 1).take(r.nextInt(N))

  def testBuild(xs: Seq[Int]) = {
    val tr = fromList(xs)
    assert(toList(tr) == xs.sortWith(_ < _), println("violate build invariant"))
    assert(isAVL(tr), println("insert: violate red-black properties"))
    assert(checkDelta(tr), println("insert: violate delta"))
  }

  def testDelete(xs: Seq[Int]) = {
    if (!xs.isEmpty) {
      xs.foldLeft((xs.sorted, fromList(xs))) {
        (t, x) => {
          val ys = t._1
          val tr = t._2
          assert(ys == toList(tr), println(s"inconsist delete result: $ys"))
          assert(isAVL(tr), println("delete: violate AVL properties"))
          assert(checkDelta(tr), println("delete: violate delata"))
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
      testDelete(xs)
    }
    println(s"$N tests passed");
  }
}
