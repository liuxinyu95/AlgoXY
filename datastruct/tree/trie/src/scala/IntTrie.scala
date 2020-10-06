/*
 * IntTrie.scala
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

object IntTrie {
  sealed trait IntTrie[+A]
  case object Empty extends IntTrie[Nothing]
  case class Br[A] (left: IntTrie[A], value: Option[A], right: IntTrie[A]) extends IntTrie[A]

  def left[A] (t: IntTrie[A]): IntTrie[A] = t match {
    case Empty => Empty
    case Br(l, _, _) => l
  }

  def right[A] (t: IntTrie[A]): IntTrie[A] = t match {
    case Empty => Empty
    case Br(_, _, r) => r
  }

  def getValue[A] (t: IntTrie[A]): Option[A] = t match {
    case Empty => None
    case Br(_, v, _) => v
  }

  def insert[A] (t: IntTrie[A], key: Int, value: A): IntTrie[A] = {
    val l = left(t)
    val r = right(t)
    val v = getValue(t)
    if (key == 0) {
      Br(l, Some(value), r)
    } else if (key % 2 == 0) {   //even
      Br(insert(l, key / 2, value), v, r)
    } else {
      Br(l, v, insert(r, key / 2, value))
    }
  }

  // look up
  def search[A] (t: IntTrie[A], key: Int) : Option[A] = t match {
    case Empty => None
    case Br(l, v, r) => if (key == 0) {
      v
    } else if (key % 2 == 0) {  //even
      search(l, key / 2)
    } else {
      search(r, key / 2)
    }
  }

  // auxiliary
  def fromList[A] (xs: Seq[(Int, A)]): IntTrie[A] =
    ((Empty: IntTrie[A]) /: xs) { (t, kv) => insert(t, kv._1, kv._2) }

  // k = ... a2, a2, a0 ==> k' = ai * m + k, where m = 2^i
  def toList[A] (t: IntTrie[A]): List[(Int, Option[A])] = {
    def toListFrom[A] (t: IntTrie[A], k: Int, m: Int): List[(Int, Option[A])] =
      t match {
        case Empty => List()
        case Br(l, v, r) => toListFrom(l, k, 2 * m) :::
          ((k, v) :: toListFrom(r, m + k, 2 * m))
      }
    toListFrom(t, 0, 1)
  }


  val N = 100
  def genList(r: Random) = r.shuffle(0 to N - 1).take(r.nextInt(N))

  def testBuild(xs: Seq[(Int, Int)]) = {
    if (!xs.isEmpty) {
      val tr = fromList(xs)
      val err = xs.filter( kv => {
        val (k, v) = kv
        search(tr, k) match {
          case Some(x) => x != v
          case None => true
        }
      })
      assert(err.isEmpty, println(err))
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
