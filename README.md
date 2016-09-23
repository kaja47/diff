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
```
