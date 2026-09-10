
-----
# <a name="fogus.treaps">fogus.treaps</a>


A Clojure implementation of persistent tree + heap = treaps
  according to the paper "Fast Set Operations Using Treaps" by Guy
  E. Blelloch and Margaret Reid-Miller.

  The tree aspect refers to the binary search tree structure imposed
  by key ordering, the lesser on the left and the greater on the
  right. A heap refers to the property of the relationship of the
  parent and descendant nodes withinn the subtrees. That is, every
  parent must be 'bigger' than its children, where 'bigger' means
  higher priority. These two orders are independent by design. A treap
  is best understood as a binary search tree on keys whose specific
  shape, among all shapes consistent with that key order, is selected
  by treating priority as a second, unrelated ordering criterion.
  
  A treap orders its elements by a custom comparator, maintaining the
  ordinary binary search tree invariant: every element in a node's
  left subtree compares less than that node, and every element in its
  right subtree compares greater. In-order traversal therefore always
  yields a total order, independent of insertion history or the tree's
  internal shape.

  Shape is governed separately, by a random priority. Each treap node
  carries a priority, and the tree is additionally kept in heap order
  with respect to priority: every node's priority exceeds those of its
  descendants. Because priority is independent of key order, the
  resulting shape is a logarithmic depth without any explicit rebalancing
  on insertion or deletion. The balance guarantee is probabilistic rather
  than structural: it holds over the distribution of priorities, not
  by construction for any single tree.

  Despite this randomness, iteration order is stable. Random
  priorities determine only where a node sits within its structurally
  valid range, never which range that is. The comparator's order
  constrains every valid shape identically. Two treaps holding the
  same elements under the same comparator therefore always traverse
  identically, regardless of priority values, insertion order, or
  history.

  Because shape follows purely from priority rather than from
  insertion history, a treap can be decomposed and recombined without
  ever needing to rebalance. Splitting at a key or joining two ordered
  treaps is a structural operation, not a repair step, since the heap
  and tree invariants remain trivially satisfiable at every point in
  the process. Set operations (union, intersection, difference) reduce
  entirely to split and join, applied recursively along the shape the
  priorities already dictate. There is no separate balancing pass, no
  bookkeeping of rotations or color bits; the randomness that gives a
  treap its expected balance is the same randomness that makes
  composing and decomposing treaps cheap.




## <a name="fogus.treaps/->TreapSet">`->TreapSet`</a><a name="fogus.treaps/->TreapSet"></a>
``` clojure

(->TreapSet metadata root __hc __heq)
```
<p><sub><a href="https://github.com/fogus/clj-treaps/blob/main/src/fogus/treaps.clj#L59-L159">Source</a></sub></p>

## <a name="fogus.treaps/EMPTY">`EMPTY`</a><a name="fogus.treaps/EMPTY"></a>



<p><sub><a href="https://github.com/fogus/clj-treaps/blob/main/src/fogus/treaps.clj#L57-L57">Source</a></sub></p>

## <a name="fogus.treaps/TreapSet">`TreapSet`</a><a name="fogus.treaps/TreapSet"></a>



<p><sub><a href="https://github.com/fogus/clj-treaps/blob/main/src/fogus/treaps.clj#L59-L159">Source</a></sub></p>

## <a name="fogus.treaps/treap-set">`treap-set`</a><a name="fogus.treaps/treap-set"></a>
``` clojure

(treap-set & elems)
```

Builds a TreapSet from elems using a random-priority policy, the
  same independent-random-priority assumption the paper's expected
  O(m log(n/m)) union/intersection/difference bounds depend on.
<p><sub><a href="https://github.com/fogus/clj-treaps/blob/main/src/fogus/treaps.clj#L163-L168">Source</a></sub></p>

-----
# <a name="fogus.treaps.set">fogus.treaps.set</a>






## <a name="fogus.treaps.set/difference">`difference`</a><a name="fogus.treaps.set/difference"></a>
``` clojure

(difference s)
(difference s1 s2)
(difference s1 s2 & more)
```

Return a treap set that is the first set without elements of the remaining treap sets
<p><sub><a href="https://github.com/fogus/clj-treaps/blob/main/src/fogus/treaps/set.clj#L26-L30">Source</a></sub></p>

## <a name="fogus.treaps.set/intersection">`intersection`</a><a name="fogus.treaps.set/intersection"></a>
``` clojure

(intersection s)
(intersection s1 s2)
(intersection s1 s2 & more)
```

Return a treap set that is the intersection of the input treap sets.
<p><sub><a href="https://github.com/fogus/clj-treaps/blob/main/src/fogus/treaps/set.clj#L20-L24">Source</a></sub></p>

## <a name="fogus.treaps.set/subset?">`subset?`</a><a name="fogus.treaps.set/subset?"></a>
``` clojure

(subset? t1 t2)
```

Is t1 a subset of t2?
<p><sub><a href="https://github.com/fogus/clj-treaps/blob/main/src/fogus/treaps/set.clj#L32-L35">Source</a></sub></p>

## <a name="fogus.treaps.set/superset?">`superset?`</a><a name="fogus.treaps.set/superset?"></a>
``` clojure

(superset? t1 t2)
```

Is set1 a superset of set2?
<p><sub><a href="https://github.com/fogus/clj-treaps/blob/main/src/fogus/treaps/set.clj#L37-L40">Source</a></sub></p>

## <a name="fogus.treaps.set/symmetric-difference">`symmetric-difference`</a><a name="fogus.treaps.set/symmetric-difference"></a>
``` clojure

(symmetric-difference t1 t2)
```

Elements in t1 or t2 but not both.
<p><sub><a href="https://github.com/fogus/clj-treaps/blob/main/src/fogus/treaps/set.clj#L42-L50">Source</a></sub></p>

## <a name="fogus.treaps.set/union">`union`</a><a name="fogus.treaps.set/union"></a>
``` clojure

(union)
(union s)
(union s1 s2)
(union s1 s2 & more)
```

Return a treap set that is the union of the input treap sets.
<p><sub><a href="https://github.com/fogus/clj-treaps/blob/main/src/fogus/treaps/set.clj#L13-L18">Source</a></sub></p>
