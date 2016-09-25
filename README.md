## diff

Little playground Scala library for diffing and patching sequences and trees.

### Usage

```scala
import atrox.diff._

val a = "This library is useless."
val b = "This library is really great."

// character level diff

val delta = diff(a, b)
assert(patch(a, delta) == b)

// word level diff

val aw = a.split(" ").toSeq
val bw = b.split(" ").toSeq

val deltaw = diff(aw, bw)
assert(patch(aw, deltaw) == bw)


// tree diff


val ta = node(Leaf(1), node(Leaf(2), Leaf(3)))
val tb = node(node(Leaf(1), Leaf(2)), Leaf(3))

val tdelta = diff(traversal(ta), traversal(tb))


assert(makeTree(patch(traversal(ta), tdelta)) == tb)
```
