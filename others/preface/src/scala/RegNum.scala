/*
 * RegNum.scala
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
/* Regular Number, generate numbers only with factors of 2, 3, 5. */
import scala.math.BigInt

object RegNum {
  lazy val ns: Stream[BigInt] = 1 #:: merge(ns map {_ * 2}, merge(ns map {_ * 3}, ns map {_ * 5}))

  /*
   * We can't use Haskell like case (x #:: xs, y #:: ys) to match
   * As the tail will be forced to evaluat, which cause stack overflow
   */

  def merge[T <% Ordered[T]](a: Stream[T], b: Stream[T]) : Stream[T] =
    if (a.head < b.head) a.head #:: merge(a.tail, b)
    else if (a.head == b.head) a.head #:: merge(a.tail, b.tail)
    else b.head #:: merge(a, b.tail)

  //ns.take(1500).last

  /*
   * 3 queues approach. Using immutable containers.
   */
  def take(n: Int) = {
    import scala.collection.immutable.Queue
    import scala.collection.immutable.Seq

    def generate(n: Int, xs: Seq[BigInt],
                 q2: Queue[BigInt], q3: Queue[BigInt], q5: Queue[BigInt]) : Seq[BigInt] = {
      if (n == 1) xs
      else {
        val x = q2.head min q3.head min q5.head
        if (x == q2.head)
          generate(n - 1, xs :+ x, q2.tail :+ 2 * x, q3 :+ 3 * x, q5 :+ 5 * x)
        else if (x == q3.head)
          generate(n - 1, xs :+ x, q2, q3.tail :+ 3 * x, q5 :+ 5 * x)
        else
          generate(n - 1, xs :+ x, q2, q3, q5.tail :+ 5 * x)
      }
    }
    generate(n, Seq(1), Queue(2), Queue(3), Queue(5))
  }

  /*
   * Using mutable queue and Seq. This is not a `pure' approach
   */
  def get(n: Int) = {
    import scala.collection.mutable.Queue
    import scala.collection.mutable.Seq

    var xs : Seq[BigInt] = Seq(1)
    val q2 : Queue[BigInt] = Queue(2)
    val q3 : Queue[BigInt] = Queue(3)
    val q5 : Queue[BigInt] = Queue(5)
    def generate(n : Int) : Seq[BigInt] = {
      if (n == 1) xs
      else {
        val x = q2.head min q3.head min q5.head
        xs = xs :+ x
        if (x == q2.head) {
          q2.dequeue
          q2 enqueue 2 * x
          q3 enqueue 3 * x
          q5 enqueue 5 * x
        } else if (x == q3.head) {
          q3.dequeue
          q3 enqueue 3 * x
          q5 enqueue 5 * x
        } else {
          q5.dequeue
          q5 enqueue 5 * x
        }
        generate(n - 1)
      }
    }
    generate(n)
  }

  //get(1500).last

  def test() = {
    val n = 100
    val xs = ns.take(n)
    val ys = take(n)
    val zs = get(n)
    assert(xs.size == ys.size && ys.size == zs.size, println("size not same"))
    assert((xs zip ys).forall{case (x, y) => x == y}, println("xs != ys"))
    assert((ys zip zs).forall{case (y, z) => y == z}, println("ys != zs"))
    println(s"$n tests passed.");
  }
}
