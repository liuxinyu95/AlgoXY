/*
 * MinFree.scala
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
/*
 * Given a list of non-negative numbers, find the minimum free number,
 * which is the minimum one not in this list.
 */

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.Random
import scala.collection.mutable.BitSet

object MinFree {

  // Divide and Conquer method

  def minFree(xs : Seq[Int]) : Int = {
    @tailrec def bsearch(xs : Seq[Int], l : Int, u : Int) : Int = {
      val m = (l + u) / 2
      val (as, bs) = xs partition (_ <= m)
      if (xs isEmpty)
        l
      else if ((as length) == m - l + 1)
        bsearch(bs, m + 1, u)
      else
        bsearch(as, l, m)
    }
    bsearch(xs, 0, xs length)
  }

  // flag array with BitSet

  val flags = BitSet()

  def minFree2(xs : Seq[Int]) : Int = {
    val n = xs.length
    flags.clear()
    for(x <- xs if x < n) flags += x
    (0 to n).find(!flags(_)) match {
      case Some(i) => i
      case None => n
    }
  }

  // verification

  val N = 1000

  def test() {
    val r = Random
    for (_ <- 1 to N) {
      val ys = r.shuffle(0 to N - 1).take(r.nextInt(N))
      val a = minFree(ys)
      val b = minFree2(ys)
      assert(a == b, println(s"d&c = $a, flags = $b"));
    }
    println(s"$N tests passed.");
  }
}
