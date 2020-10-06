/*
 * RBTree.scala
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
import scala.util.Random   //for verification
import scala.language.postfixOps

object RBTree {
  sealed abstract class Color
  case object R extends Color
  case object B extends Color
  case object BB extends Color  //doubly black

  sealed trait Tree[+A]
  case object E extends Tree[Nothing]
  case object BBE extends Tree[Nothing] //doubly black empty
  case class T[A] (c: Color, left: Tree[A], key: A, right: Tree[A]) extends Tree[A]

  def insert[A <% Ordered[A]] (tr: Tree[A], x: A): Tree[A] = makeBlack(ins(tr, x))

  def ins[A <% Ordered[A]] (tr: Tree[A], x: A): Tree[A] = tr match {
    case E => T(R, E, x, E)
    case T(c, left, y, right) =>
      if (x < y) balance(c, ins(left, x), y, right)
      else balance(c, left, y, ins(right, x))
    case _ => throw new RuntimeException("shouldn't be here")
  }

  def makeBlack[A](tr: Tree[A]): Tree[A] = tr match {
    case T(_, l, k, r) => T(B, l, k, r)
    case _ => E
  }

  def balance[A](c: Color, left: Tree[A], key: A, right: Tree[A]) : Tree[A] =
    (c, left, key, right) match {
      case (B, T(R, T(R, a, x, b), y, c), z, d) => T(R, T(B, a, x, b), y, T(B, c, z, d))
      case (B, T(R, a, x, T(R, b, y, c)), z, d) => T(R, T(B, a, x, b), y, T(B, c, z, d))
      case (B, a, x, T(R, b, y, T(R, c, z, d))) => T(R, T(B, a, x, b), y, T(B, c, z, d))
      case (B, a, x, T(R, T(R, b, y, c), z, d)) => T(R, T(B, a, x, b), y, T(B, c, z, d))
      case _ => T(c, left, key, right)
    }

  def delete[A <% Ordered[A]](tr: Tree[A], x: A): Tree[A] = makeBlack(del(tr, x))

  def del[A <% Ordered[A]](tr: Tree[A], x: A): Tree[A] = tr match {
    case E => E
    case T(color, l, k, r) =>
      if (x < k)
        fixDel(color, del(l, x), k, r)
      else if (x > k)
        fixDel(color, l, k, del(r, x))
      else if (isEmpty(l))
        color match {
          case B => addBlack(r)
          case _ => r
        }
      else if (isEmpty(r))
        color match {
          case B => addBlack(l)
          case _ => l
        }
      else {
        val k1 = min(r)
        fixDel(color, l, k1, del(r, k1))
      }
    case _ => throw new RuntimeException("shouldn't be here")
  }

  def isEmpty[A](tr: Tree[A]): Boolean = tr match {
    case E => true
    case _ => false
  }

  def addBlack[A](tr: Tree[A]): Tree[A] = tr match {
    case T(B, l, k, r) => T(BB, l, k, r)
    case T(_, l, k, r) => T(B, l, k, r)
    case E => BBE
    case _ => tr
  }

  def min[A] (tr: Tree[A]): A = tr match {
    case T(_, E, x, _) => x
    case T(_, left, _, _) => min(left)
  }

  def fixDel[A] (color: Color, left: Tree[A], key: A, right: Tree[A]) : Tree[A] =
    (color, left, key, right) match {
      // the sibling is black, and has one red nephew
      case (color, T(BB, _, _, _), x, T(B, T(R, b, y, c), z, d))
        => T(color, T(B, addBlack(left), x, b), y, T(B, c, z, d))
      case (color, BBE, x, T(B, T(R, b , y, c), z, d))
        => T(color, T(B, E, x, b), y, T(B, c, z, d))
      case (color, T(BB, _, _, _), x, T(B, b, y, T(R, c, z, d)))
        => T(color, T(B, addBlack(left), x, b), y, T(B, c, z, d))
      case (color, BBE, x, T(B, b, y, T(R, c, z, d)))
        => T(color, T(B, E, x, b), y, T(B, c, z, d))
      case (color, T(B, a, x, T(R, b, y, c)), z, T(BB, _, _, _))
        => T(color, T(B, a, x, b), y, T(B, c, z, addBlack(right)))
      case (color, T(B, a, x, T(R, b, y, c)), z, BBE)
        => T(color, T(B, a, x, b), y, T(B, c, z, E))
      case (color, T(B, T(R, a, x, b), y, c), z, T(BB, _, _, _))
        => T(color, T(B, a, x, b), y, T(B, c, z, addBlack(right)))
      case (color, T(B, T(R, a, x, b), y, c), z, BBE)
        => T(color, T(B, a, x, b), y, T(B, c, z, E))
      // the sibling is red
      case (B, T(BB, _, _, _), x, T(R, b, y, c))
        => fixDel(B, fixDel(R, left, x, b), y, c)
      case (B, BBE, x, T(R, b, y, c))
        => fixDel(B, fixDel(R, BBE, x, b), y, c)
      case (B, T(R, a, x, b), y, T(BB, _, _, _))
        => fixDel(B, a, x, fixDel(R, b, y, right))
      case (B, T(R, a, x, b), y, BBE)
        => fixDel(B, a, x, fixDel(R, b, y, BBE))
      // the slibing and 2 newphews are all black, propagate the blackness up
      case (color, T(BB, _, _, _), x, T(B, b, y, c))
        => addBlack(T(color, addBlack(left), x, T(R, b, y, c)))
      case (color, BBE, x, T(B, b, y, c))
        => addBlack(T(color, E, x, T(R, b, y, c)))
      case (color, T(B, a, x, b), y, T(BB, _, _, _))
        => addBlack(T(color, T(R, a, x, b), y, addBlack(right)))
      case (color, T(B, a, x, b), y, BBE)
        => addBlack(T(color, T(R, a, x, b), y, E))
      // otherwise
      case _ => T(color, left, key, right)
    }

  def fromList [A <% Ordered[A]] (xs: Seq[A]) : Tree[A] = ((E: Tree[A]) /: xs) (insert)

  def toList[A] (tr: Tree[A]) : List[A] = tr match {
    case E => List()
    case T(_, left, x, right) => toList(left) ::: (x :: toList(right))
    case _ => throw new RuntimeException("shouldn't be here")
  }

  // test
  def isBlack[A](tr: Tree[A]) : Boolean = tr match {
    case E => true
    case T(B, _, _, _) => true
    case _ => false
  }

  def adjacentRed[A](tr: Tree[A]) : Boolean = tr match {
    case E => false
    case T(R, T(R, _, _, _), _, _) => true
    case T(R, _, _, T(R, _, _, _)) => true
    case T(_, left, _, right) => adjacentRed(left) || adjacentRed(right)
  }

  def eqBlack[A](tr: Tree[A]) = blackness(tr) > 0

  def blackness[A](tr: Tree[A]) : Int = tr match {
    case E => 1
    case T(c, left, _, right) => {
      val a = blackness(left)
      val b = blackness(right)
      val i = if (isBlack(tr)) 1 else 0
      if (a != b) -1000 else a + i
    }
    case _ => throw new RuntimeException("shouldn't be here")
  }

  def isRedBlack[A](tr: Tree[A]) = isBlack(tr) && (!adjacentRed(tr)) && eqBlack(tr)

  val N = 100
  def genList(r: Random) = r.shuffle(0 to N - 1).take(r.nextInt(N))

  def testBuild(xs: Seq[Int]) = {
    val tr = fromList(xs)
    assert(toList(tr) == xs.sortWith(_ < _), println("violate build invariant"))
    assert(isRedBlack(tr), println("violate red-black properties"))
  }

  def testDelete(xs: Seq[Int]) = {
    if (!xs.isEmpty) {
      xs.foldLeft((xs.sorted, fromList(xs))) {
        (t, x) => {
          val ys = t._1
          val tr = t._2
          assert(ys == toList(tr), println(s"inconsist delete result: $ys"))
          assert(isRedBlack(tr), println("violate red-black properties"))
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
