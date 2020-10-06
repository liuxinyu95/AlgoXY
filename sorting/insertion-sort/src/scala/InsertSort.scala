/*
 * InsertSort.scala
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
import scala.util.Random          //for verification purpose
import scala.language.postfixOps

object InsertSort {
  def insert[A <% Ordered[A]] (xs : List[A], x : A) : List[A] =
    xs match {
      case List() => List(x)
      case y :: ys => if (x < y ) x :: xs else y :: insert(ys, x)
    }

  def isort[A <% Ordered[A]] (xs : List[A]) : List[A] =
    xs match {
      case List() => List()
      case y :: ys => insert(isort(ys), y)
    }

  def insertSort[A <% Ordered[A]] (xs : List[A]) : List[A] =
    ((List(): List[A]) /: xs) (insert)

  /* verification */
  val N = 100
  def genList(r: Random) = r.shuffle(0 to N - 1).take(r.nextInt(N)).toList

  def testSort[A <% Ordered[A]] (xs: List[A], f: (List[A]) => List[A], msg: String) =
    assert(f(xs) == xs.sorted, println(s"error in $msg sort $xs"))

  def test() = {
    val r = Random
    for (_ <- 1 to N) {
      val xs = genList(r)
      testSort(xs, isort[Int], "isort")
      testSort(xs, insertSort[Int], "insertSort")
    }
    println(s"pass $N tests.");
  }
}
