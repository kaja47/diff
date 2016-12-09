package atrox

import collection.mutable.ArrayBuilder
import collection.mutable.ArrayBuffer
import math.min

object diff {

  sealed trait Diff[T] { def pos: Int }
  case class Add[T](pos: Int, values: Seq[T]) extends Diff[T]
  case class Del[T](pos: Int, length: Int) extends Diff[T]


  // Longest common subsequence
  def lcs[T](a: IndexedSeq[T], b: IndexedSeq[T]): IndexedSeq[T] = {
    val alen = a.length
    val blen = b.length

    val lengths: Array[Array[Int]] = Array.fill(alen+1) { new Array[Int](blen+1) }

    // row 0 and column 0 are initialized to 0 already

    var i = 0; while (i < alen) {
      var j = 0; while (j < blen) {
        if (a(i) == b(j)) {
          lengths(i+1)(j+1) = lengths(i)(j) + 1
        } else {
          lengths(i+1)(j+1) = math.max(lengths(i+1)(j), lengths(i)(j+1))
        }

        j += 1
      }
      i += 1
    }

    // read the substring out from the matrix
    var res = ArrayBuffer[T]()
    var x = alen
    var y = blen
    while (x != 0 && y != 0) {
      if (lengths(x)(y) == lengths(x-1)(y))
        x -= 1
      else if (lengths(x)(y) == lengths(x)(y-1))
        y -= 1
      else {
        assert(a(x-1) == b(y-1))
        res += a(x-1)
        x -= 1
        y -= 1
      }
    }

    res.reverse
  }



  def diff[T](a: IndexedSeq[T], b: IndexedSeq[T]): IndexedSeq[Diff[T]] = {

    val diffs = ArrayBuffer[Diff[T]]()
    val cs = lcs(a, b)
    var apos, bpos = 0

    for (c <- cs) {
      val aDiffPos = apos

      var dels = 0
      while (apos < a.length && a(apos) != c) {
        dels += 1
        apos += 1
      }
      if (dels > 0) {
        diffs += Del(aDiffPos, dels)
      }
      apos += 1

      val adds = ArrayBuffer[T]()
      while (bpos < b.length && b(bpos) != c) {
        adds += b(bpos)
        bpos += 1
      }
      if (adds.length > 0) {
        diffs += Add(aDiffPos, adds)
      }
      bpos += 1
    }

    val aDiffPos = apos

    var dels = 0
    while (apos < a.length) {
      dels += 1
      apos += 1
    }
    if (dels > 0) {
      diffs += Del(aDiffPos, dels)
    }

    val adds = ArrayBuffer[T]()
    while (bpos < b.length) {
      adds += b(bpos)
      bpos += 1
    }
    if (adds.length > 0) {
      diffs += Add(aDiffPos, adds)
    }

    diffs
  }


  def patch[T](src: Seq[T], diffs: Seq[Diff[T]]): Seq[T] = {

    var builderLen = src.length
    diffs foreach {
      case Del(pos, len) => builderLen -= len
      case Add(pos, data) => builderLen += data.length
    }

    val res = new ArrayBuffer[T](builderLen)
    var srcPos = 0

    diffs.sortBy(d => (d.pos, !d.isInstanceOf[Add[_]])) foreach { // deletions first
      case Del(pos, len) =>
        if (srcPos < pos) {
          res ++= src.slice(srcPos, pos)
        }
        srcPos = pos + len
      case Add(pos, values) =>
        if (srcPos < pos) {
          res ++= src.slice(srcPos, pos)
        }
        res ++= values
        srcPos = pos
    }

    if (srcPos < src.length) {
      res ++= src.slice(srcPos, src.length)
    }

    assert(res.length == builderLen)

    res
  }


  // tree diffs


  sealed trait Tree[+T]
  case class Node[+T](children: Seq[Tree[T]]) extends Tree[T]
  case class Leaf[+T](value: T) extends Tree[T]

  def node[T](children: Tree[T]*) = Node(children)

  sealed trait Token[+T]
  case object Enter extends Token[Nothing]
  case object Leave extends Token[Nothing]
  case class Value[T](value: T) extends Token[T]


  def traversal[T](tree: Tree[T]): Seq[Token[T]] = {
    tree match {
      case Node(children) => Enter +: (children flatMap traversal) :+ Leave
      case Leaf(value) => Vector(Value(value))
    }
  }

  private def mkSeq[T](nodes: Seq[Tree[T]], tokens: Seq[Token[T]]): (Seq[Tree[T]], Seq[Token[T]]) = {
    tokens.head match {
      case Enter =>
        val (n, rest) = mkNode(tokens.tail)
        mkSeq(nodes :+ n, rest)
      case Leave => (nodes, tokens.tail)
      case Value(v) => mkSeq(nodes :+ Leaf(v), tokens.tail)
    }
  }

  private def mkNode[T](tokens: Seq[Token[T]]): (Tree[T], Seq[Token[T]]) = {
    val (nodes, rest) = mkSeq(Seq(), tokens)
    (Node(nodes), rest)
  }


  def makeTree[T](tokens: Seq[Token[T]]): Tree[T] = {
    require(tokens.head match { case Enter => true ; case _ => false })
    val (tree, rest) = mkNode(tokens.tail)
    require(rest.isEmpty, "rest is not empty")
    tree
  }




  // Dynamic time warping


  def dtwMatrix[T](a: IndexedSeq[T], b: IndexedSeq[T], dist: (T, T) => Double): Array[Array[Double]] = {
    val alen = a.length
    val blen = b.length

    val dtw: Array[Array[Double]] = Array.fill(alen+1) { new Array[Double](blen+1) }

    for (i <- 1 to alen) dtw(i)(0) = Double.PositiveInfinity
    for (i <- 1 to blen) dtw(0)(i) = Double.PositiveInfinity

    for (i <- 1 to alen ; j <- 1 to blen) {
      val cost = dist(a(i-1), b(j-1))
      dtw(i)(j) = cost + min(min(dtw(i-1)(j  ),  // insertion
                                 dtw(i  )(j-1)), // deletion
                                 dtw(i-1)(j-1))  // match
    }

    dtw
  }

  def readDtwPath(dtw: Array[Array[Double]]): List[(Int, Int)] = {
    val alen = dtw.length - 1
    val blen = dtw(0).length - 1

    // read the substring out from the matrix
    var x = alen
    var y = blen
    var res = List[(Int, Int)]()

    while (x != 0 && y != 0) {
      res = (x-1, y-1) :: res

      val aaa = dtw(x-1)(y  ) // insertion
      val bbb = dtw(x  )(y-1) // deletion
      val dia = dtw(x-1)(y-1) // match

       if (aaa <= dia && aaa <= bbb) {
        x -= 1
      } else if (bbb <= dia && bbb <= aaa) {
        y -= 1
      } else {
        require(dia <= aaa && dia <= bbb)
        x -= 1
        y -= 1
      }
    }

    res
  }

  def readDtwPath[T](dtw: Array[Array[Double]], a: IndexedSeq[T], b: IndexedSeq[T]): List[(T, T)] =
    readDtwPath(dtw) map { case (i,j) => (a(i), b(j)) }

  /** Dynamic time warping */
  def dtw[T](a: IndexedSeq[T], b: IndexedSeq[T], dist: (T, T) => Double): List[(T, T)] =
    readDtwPath(dtwMatrix(a, b, dist), a, b)

  def lcsViaDtw[T](a: IndexedSeq[T], b: IndexedSeq[T]): Seq[T] =
    readDtwPath(dtwMatrix(a, b, (a: T, b: T) => if (a == b) -1 else 0)) collect { case (i, j) if a(i) == b(j) => a(i) }


  def levenshtein[T](as: IndexedSeq[T], bs: IndexedSeq[T]): Int = {
    val f = new Array[Int](bs.length+1)

    var j = 0 ; while (j < f.length) {
      f(j) = j
      j += 1
    }

    for (a <- as) {
      var j = 1
      // fj1 is the value of f[j - 1] in last iteration
      var fj1 = f(0)
      f(0) += 1
      for (cb <- bs) {
        var mn = min(f(j)+1, f(j-1)+1) // delete & insert
        if (cb != a) {
          mn = min(mn, fj1+1) // change
        } else {
          mn = min(mn, fj1) // matched
        }

        // save f[j] to fj1(j is about to increase), update f[j] to mn
        //fj1, f(j) = f(j), mn
        fj1 = f(j)
        f(j) = mn
        j += 1
      }
    }

    f(f.length-1)
  }

}
