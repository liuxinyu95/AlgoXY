/*
 * MissDup.scala
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
 * Given numbers from 1 to n, after some processing, there are some changes.
 *  1) The order is shuffled;
 *  2) One number x is mutated to y, here both x, y are from 1 to n.
 * Develop a method to find the x and y in linear time with constant space.
 *
 * Examples
 * [3, 1, 3, 5, 4] ==> x = 2, y = 3
 */

object MissDup {

  // divide and conquer

  def missDup(xs : List[Int]) : (Int, Int) = {
    def solve(xs : List[Int], l : Int, u : Int) : (Int, Int) = {
      val m = (l + u) / 2
      val (as, bs) = xs partition (_ <= m)
      val k = as length
      val sl = (l + m) * (m - l + 1) / 2
      val sr = (m + 1 + u) * (u - m) / 2
      val sl1 = as sum
      val sr1 = bs sum
      val result =
        if (k < m - l + 1)
           (sl - sl1, sr1 - sr)
        else if (k > m - l + 1)
          (sr - sr1, sl1 - sl)
        else if (sl == sl1)
          solve(bs, m + 1, u)
        else
          solve(as, l, m)
      result
    }
    solve(xs, 1, xs length)
  }

  // verification

  def test() {
    val r = scala.util.Random
    for (_ <- 1 to 100) {
      val n = scala.math.max(r.nextInt(100), 2)
      val xs = r shuffle(1 to n)
      val x = xs(0)
      val y = xs(scala.math.max(r.nextInt(n), 1))
      val (m, d) = missDup(r shuffle(y :: xs.toList.tail) toList)
      assert(m == x && d == y, println(s"$m == $x and $d == $y fail."));
    }
    println("100 tests passed");
  }
}
