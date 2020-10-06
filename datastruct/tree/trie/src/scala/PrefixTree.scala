import scala.language.postfixOps
import Ordering.Implicits._ // for list comparison

object PrefixTree {
  case class Tree[+K, V] (v: Option[V], cs: List[(List[K], Tree[K, V])]) {
    val value = v
    val children = cs
  }

  def empty[K, V](): Tree[K, V] = Tree(None, List())

  def leaf[K, V](v: V): Tree[K, V] = Tree(Some(v), List())

  def insert[K, V] (t: Tree[K, V], key: List[K], v: V): Tree[K, V] = {
    def ins[K, V] (ts: List[(List[K], Tree[K, V])],
                   key: List[K], v: V): List[(List[K], Tree[K, V])] =
      ts match {
        case List() => List((key, leaf(v)))
        case p :: ps => {
          val (key1, t1) = p
          if (key1 == key) {
            (key, Tree(Some(v), t1.children)) :: ps  // overwrite
          } else if (hasPrefix(key1, key)) {
            branch(key, v, key1, t1) :: ps
          } else {
            p :: ins(ps, key, v)
          }
        }
      }
    Tree(t.value, ins(t.children, key, v))
  }

  def hasPrefix[K] (key1: List[K], key2: List[K]) =
    ! key1.isEmpty && ! key2.isEmpty && key1.head == key2.head

  def branch[K, V] (key1: List[K], v: V,
                    key2: List[K], t2: Tree[K, V]): (List[K], Tree[K, V]) = {
    val key = lcp(key1, key2)
    val m = key.length
    val newKey1 = key1.drop(m)
    val newKey2 = key2.drop(m)
    if (key1 == key) { // insert "an" into "another"
      (key, Tree(Some(v), List((newKey2, t2))))
    } else if (key2 == key) { // insert "another" into "an"
      (key, insert(t2, newKey1, v))
    } else {
      (key, Tree(None, List((newKey1, leaf(v)), (newKey2, t2))))
    }
  }

  // the longest common prefix
  def lcp[K] (xs: List[K], ys: List[K]): List[K] =
    if (!xs.isEmpty && !ys.isEmpty && xs.head == ys.head) {
      xs.head :: lcp(xs.tail, ys.tail)
    } else {
      List()
    }

  // lookup
  def lookup[K, V] (t: Tree[K, V], key: List[K]): Option[V] = {
    def diff[K] (xs: List[K], ys: List[K]) = xs.drop(lcp(xs, ys).length)

    def find[K, V] (assoc: List[(List[K], Tree[K, V])], key: List[K]): Option[V] =
      assoc match {
        case List() => None
        case p :: ps => {
          val (key1, t1) = p
          if (key1 == key) {
            t1.value
          } else if (key.startsWith(key1)) {
            lookup(t1, diff(key, key1))
          } else {
            find(ps, key)
          }
        }
      }
    find(t.children, key)
  }

  def lookupString[V] (t: Tree[Char, V], key: String): Option[V] = lookup(t, key.toList)

  def fromList[K, V] (assoc: List[(List[K], V)]): Tree[K, V] =
    ((empty(): Tree[K, V]) /: assoc) { (t, p) => insert(t, p._1, p._2) }

  def fromStringList[V](assoc: List[(String, V)]): Tree[Char, V] =
    fromList(assoc map (p => (p._1.toList, p._2)))

  def fromString(txt: String): Tree[Char, Int] =
    fromStringList(txt.split("\\s+").toList.zipWithIndex)

  // pre-order travrese to populate keys in lexicographical order
  def keys[K <% Ordered[K], V] (t: Tree[K, V]): List[List[K]] = {
    def keysOfPrefix[K <% Ordered[K], V](prefix: List[K],
                                         t: Tree[K, V]): List[List[K]] = {
      val ts = t.children.sortWith(_._1 < _._1)
      val ks = ts flatMap ((p: (List[K], Tree[K, V])) =>
        keysOfPrefix(prefix ++ p._1, p._2))
      t.value match {
        case None => ks
        case Some(_) => prefix :: ks
      }
    }
    keysOfPrefix(List(), t)
  }

  def stringKeys[K <% Ordered[K], V] (t: Tree[K, V]): List[String] =
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
