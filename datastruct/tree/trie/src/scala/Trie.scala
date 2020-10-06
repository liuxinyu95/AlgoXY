/*
 * Trie.scala
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
import scala.util.Random    // for verification
import scala.language.postfixOps

object Trie {
  case class Trie[+K, V] (v: Option[V], cs: List[(K, Trie[K, V])]) {
    val value = v
    val children = cs
  }

  def empty[K, V](): Trie[K, V] = Trie(None, List())

  def insert[K, V] (t: Trie[K, V], key: List[K], value: V): Trie[K, V] = {
    def ins[K, V] (ts: List[(K, Trie[K, V])],
                   k: K, ks: List[K], value: V): List[(K, Trie[K, V])] =
      ts match {
        case List() => List((k, insert(empty(), ks, value)))
        case p :: ps => if (p._1 == k)
                           (k, insert(p._2, ks, value)) :: ps
                        else
                          p :: ins(ps, k, ks, value)
      }
    key match {
      case List() => Trie(Some(value), t.children)
      case k :: ks => Trie(t.value, ins(t.children, k, ks, value))
    }
  }

  //lookup
  def lookup[K, V] (t: Trie[K, V], key: List[K]): Option[V] = {
    def find[K, V] (x: K, assoc: List[(K, V)]): Option[V] = assoc match {
      case List() => None
      case p :: ps => if (p._1 == x) Some(p._2) else find(x, ps)
    }
    key match {
      case List() => t.value
      case k :: ks => find(k, t.children) match {
        case None => None
        case Some(tr) => lookup(tr, ks)
      }
    }
  }

  def lookupString[V] (t: Trie[Char, V], key: String): Option[V] =
    lookup(t, key.toList)

  def fromList[K, V](assoc: List[(List[K], V)]): Trie[K, V] =
    ((empty():Trie[K, V]) /: assoc) { (t, p) => insert(t, p._1, p._2) }

  def fromStringList[V](assoc: List[(String, V)]): Trie[Char, V] =
    fromList(assoc map (p => (p._1.toList, p._2)))

  // Pre-order traverse to populate keys in lexicographical order
  def keys[K <% Ordered[K], V] (t: Trie[K, V]): List[List[K]] = {
    def keysOfPrefix[K <% Ordered[K], V](t: Trie[K, V],
                                         prefix: List[K]): List[List[K]] = {
      val ts = t.children.sortWith(_._1 < _._1)
      val ks = ts flatMap ((p: (K, Trie[K, V])) =>
                           keysOfPrefix(p._2, p._1 :: prefix))
      t.value match {
        case None => ks
        case Some(_) => prefix :: ks
      }
    }
    keysOfPrefix(t, List()) map (_.reverse)
  }

  def stringKeys[K <% Ordered[K], V](t: Trie[K, V]): List[String] =
    keys(t) map (_.mkString)

  // verification

  // test data
  val assocs = List(List(("a", 1), ("an", 2), ("antoher", 7), ("boy", 3), ("bool", 4), ("zoo", 3)),
                    List(("zoo", 3), ("bool", 4), ("boy", 3), ("another", 7), ("an", 2), ("a", 1)))

  def testBuild(kvs: List[(String, Int)]) = {
    val t = fromStringList(kvs)
    val err = kvs.filter( kv => {
      val (k, v) = kv
      lookupString(t, k) match {
        case Some(x) => x != v
        case None => true
      }
    })
    assert(err.isEmpty, println("err\n" + t.toString))
  }

  def testKeys(kvs: List[(String, Int)]) = {
    val t = fromStringList(kvs)
    val ks1 = stringKeys(t)
    val ks2 = kvs.unzip._1.sortWith(_<_)
    assert(ks1 == ks2, println("ks1=" + ks1.toString + "\nks2=", ks2.toString))
    println("t=" + t.toString + "\nkeys=" + ks1.toString)
  }

  def test() = {
    for (assoc <- assocs) {
      testBuild(assoc)
      testKeys(assoc)
    }
    println("pass " + assocs.size + " tests")
  }
}
