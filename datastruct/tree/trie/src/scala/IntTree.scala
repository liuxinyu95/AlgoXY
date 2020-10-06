/*
 * IntTree.scala
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

object IntTree {
  sealed trait IntTree[+A]
  case object Empty extends IntTree[Nothing]
  case class Leaf[A] (key: Int, value: A) extends IntTree[A]
  case class Branch[A] (prefix: Int, mask: Int,
                        left: IntTree[A], right: IntTree[A]) extends IntTree[A]

  //Auxiliary

  /*
   * Mask a number with a mask, where
   *   number x = a(n) a(n-1) ... a(i) a(i-1) ... a(0)
   *   mask   m = 100...0  (= 2^i)
   *   result: a(n) a(n-1) ... a(i) 00...0
   */
  def maskbit(x: Int, mask: Int): Int = x & (~(mask - 1))

  /*
   * Test if the next bit after mask bit is zero
   * e.g. number x = a(n) a(n-1) ... a(i) b ...a(0)
   *      mask   m = 100..0 (= 2^i)
   *  returns true if b = 0; false if b = 1
   */
  def isZero(x: Int, mask: Int): Boolean = (x & (mask >> 1)) == 0

  //find the longest common prefix, returns a pair: (prefix, mask)
  def lcp(p1: Int, p2: Int): (Int, Int) = {
    def nbits(x: Int) : Int = if (x == 0) 0 else (1 + nbits(x >> 1))
    val mask = 1 << nbits(p1 ^ p2)
    val prefix = maskbit(p1, mask)
    (prefix, mask)
  }

  def join[A] (p1: Int, t1: IntTree[A], p2: Int, t2: IntTree[A]): IntTree[A] = {
    val (p, m) = lcp(p1, p2)
    if (isZero(p1, m)) {
      Branch(p, m, t1, t2)
    } else {
      Branch(p, m, t2, t1)
    }
  }

  def matchBits(key: Int, prefix: Int, mask: Int): Boolean = maskbit(key, mask) == prefix

  def insert[A] (tr: IntTree[A], key: Int, value: A): IntTree[A] = tr match {
    case Empty => Leaf(key, value)
    case Leaf(k, v) => {
      val t = Leaf(key, value)
      if (key == k) t else join(key, t, k, tr)
    }
    case Branch(p, m, left, right) => {
      if (matchBits(key, p, m)) {
        if (isZero(key, m)) {
          Branch(p, m, insert(left, key, value), right)
        } else {
          Branch(p, m, left, insert(right, key, value))
        }
      } else {
        join(key, Leaf(key, value), p, tr)
      }
    }
  }

  // look up
  def lookup[A] (tr: IntTree[A], key: Int): Option[A] = tr match {
    case Empty => None
    case Leaf(k, v) => if (key == k) Some(v) else None
    case Branch(p, m, left, right) =>
      if (matchBits(key, p, m))
        lookup(if (isZero(key, m)) left else right, key)
      else
        None
  }

  def fromList[A] (xs: Seq[(Int, A)]): IntTree[A] =
    ((Empty: IntTree[A]) /: xs) { (t, kv) => insert(t, kv._1, kv._2) }

  def toList[A] (tr: IntTree[A]): List[(Int, Option[A])] = tr match {
    case Empty => List()
    case Leaf(k, v) => List((k, Some(v)))
    case Branch(p, m, left, right) => toList(left) ::: ((p, None) :: toList(right))
  }

  def toString[A] (tr: IntTree[A]): String = tr match {
    case Empty => "."
    case Leaf(k, v) => k.toString() + ":" + v.toString
    case Branch(p, m, l, r) => "[" + p.toString() + "@" + m.toString() + "]" +
      "(" + toString(l) + ", " + toString(r) + ")"
  }

  //verification
  val N = 100
  def genList(r: Random) = r.shuffle(0 to N - 1).take(r.nextInt(N))

  def testBuild(xs: Seq[(Int, Int)]) = {
    if (!xs.isEmpty) {
      val tr = fromList(xs)
      val err = xs.filter( kv => {
        val (k, v) = kv
        lookup(tr, k) match {
          case Some(x) => x != v
          case None => true
        }
      })
      assert(err.isEmpty, println("err\n" + toString(tr)))
    }
  }

  def test() = {
    val r = Random
    for (_ <- 1 to N) {
      val xs = r.shuffle(genList(r) zip (Stream from 1))
      testBuild(xs)
    }
    println(s"$N tests passed")
  }
}
