(ns fogus.treaps
  "A Clojure implementation of persistent tree + heap = treaps
  according to the paper \"Fast Set Operations Using Treaps\"
  by Guy E. Blelloch and Margaret Reid-Miller. 

  The tree aspect refers to the binary search tree structure
  imposed by key ordering, the lesser on the left and the
  greater on the right. A heap refers to the property of the
  relationship of the parent and descendant nodes withinn the
  subtrees. That is, every parent must be 'bigger' than its
  children, where 'bigger' means higher priority. These two
  orders are independent by design. A treap is best understood
  as a binary search tree on keys whose specific shape, among all
  shapes consistent with that key order, is selected by treating
  priority as a second, unrelated ordering criterion.
  
  A treap orders its elements by a custom comparator, maintaining
  the ordinary binary search tree invariant: every element in a
  node's left subtree compares less than that node, and every
  element in its right subtree compares greater. In-order traversal
  therefore always yields a total order, independent of insertion
  history or the tree's internal shape.

  Shape is governed separately, by a random priority. Each treap node
  carries a priority, and the tree is additionally kept in heap order
  with respect to priority: every node's priority exceeds those of its
  descendants. Because priority is independent of key order, the
  resulting shape is a logarithmic depth without any explicit rebalancing
  on insertion or deletion. The balance guarantee is probabilistic rather
  than structural: it holds over the distribution of priorities, not
  by construction for any single tree.

  Despite this randomness, iteration order is stable. Random priorities
  determine only where a node sits within its structurally valid range,
  never which range that is. The comparator's order constrains every valid
  shape identically. Two treaps holding the same elements under the
  same comparator therefore always traverse identically, regardless of
  priority values, insertion order, or history.

  Because shape follows purely from priority rather than from insertion
  history, a treap can be decomposed and recombined without ever needing to
  rebalance. Splitting at a key or joining two ordered treaps is a structural
  operation, not a repair step, since the heap and tree invariants remain
  trivially satisfiable at every point in the process. Set operations (union,
  intersection, difference) reduce entirely to split and join, applied
  recursively along the shape the priorities already dictate. There is no
  separate balancing pass, no bookkeeping of rotations or color bits; the
  randomness that gives a treap its expected balance is the same randomness
  that makes composing and decomposing treaps cheap."
  (:require [fogus.treaps.impl :as impl]))

;; TODO: toArray/1
;; TODO: cached hashes
;; TODO: docs
;; TODO: tail-recursive split/diff/etc. impls

(set! *warn-on-reflection* true)

(def EMPTY impl/EMPTY_TREAP)

(deftype TreapSet [metadata root]
  clojure.lang.IObj
  (withMeta [_ m]
    (TreapSet. m root))

  clojure.lang.IMeta
  (meta [_] metadata)
  
  impl/TreapSPI
  ;; TreapSet exposes just k to the outside world since it's always the same
  ;; as v anyway.
  (-materialize-kv [_ k _] k)
  ;; Treats the obj itself as both k and v.
  (-unwrap-kv [_ obj] [obj obj])
  
  clojure.lang.Seqable
  (seq [this] (impl/walk this seq root))

  java.lang.Object
  (equals [this other]
    (clojure.lang.APersistentSet/setEquals this other))
  (hashCode [this]
    (clojure.lang.Murmur3/hashUnordered this))

  clojure.lang.IPersistentCollection
  (cons [this obj]
    (let [[k v] (impl/-unwrap-kv this obj)]
      (TreapSet. metadata (impl/-insert root (impl/random-priority k) k v))))
  (empty [_] (TreapSet. metadata EMPTY))
  (equiv [this other]
    (cond
      (or (instance? clojure.lang.IPersistentSet other) (instance? java.util.Set other))
      (and (= (count this) (count other))
           (every? #(.contains ^java.util.Set other %) (seq this)))
      :else false))

  clojure.lang.IPersistentSet
  (disjoin [_ key]
    (let [[l _ g] (impl/-split root key)]
      (TreapSet. metadata (impl/-join l g))))
  (get [_ key]
    (impl/-find root key))

  impl/PersistentTreap
  (-split [_ _]
    (throw (UnsupportedOperationException. "TreapSet does not support -split")))
  (-join [_ _]
    (throw (UnsupportedOperationException. "TreapSet does not support -join")))
  (-union [_ other]
    (TreapSet. metadata (or (impl/-union root (impl/-derive-root other)) EMPTY)))
  (-intersect [_ other]
    (TreapSet. metadata (or (impl/-intersect root (impl/-derive-root other)) EMPTY)))
  (-diff [_ other]
    (TreapSet. metadata (or (impl/-diff root (impl/-derive-root other)) EMPTY)))

  impl/TreapRoot
  (-derive-root [_] root)

  clojure.lang.Counted
  (count [_] (count root))

  clojure.lang.IFn
  (invoke [this key]
    (get this key))
  (invoke [this key not-found]
    (get this key not-found))

  java.util.Set
  (add [_ _] (throw (UnsupportedOperationException.)))
  (addAll [_ _] (throw (UnsupportedOperationException.)))
  (clear [_] (throw (UnsupportedOperationException.)))
  (remove [_ _] (throw (UnsupportedOperationException.)))
  (removeAll [_ _] (throw (UnsupportedOperationException.)))
  (retainAll [_ _] (throw (UnsupportedOperationException.)))
  (contains [this o]
    (boolean (some #(= % o) (seq this))))
  (containsAll [this coll]
    (every? #(.contains this %) coll))
  (isEmpty [this] (empty? (seq this)))
  (size [this] (count this))
  (toArray [this] (into-array Object this))
  (^objects toArray [this ^objects arr]) ;; TODO

  java.lang.Iterable
  (iterator [this]
    (clojure.lang.SeqIterator. (seq this)))

  clojure.lang.IHashEq
  (hasheq [this]
    (clojure.lang.Murmur3/hashUnordered this)))

;; Printing

(defmethod print-method fogus.treaps.impl.Treap
  [^fogus.treaps.impl.Treap t ^java.io.Writer w]
  (.write w (str "#T{"))  (print-method (.p t) w)
  (.write w (str " ("))   (print-method (.k t) w)
  (.write w (str " => ")) (print-method (.v t) w)
  (.write w (str ")"))
  (.write w (str " "))   (print-method (.L t) w)
  (.write w (str " "))   (print-method (.R t) w)
  (.write w "}"))

(defmethod print-method TreapSet
  [^TreapSet ts ^java.io.Writer w]
  (.write w "#TreapSet")
  (print-method (.root ts) w))

(defmethod print-method fogus.treaps.impl.EmptyTreap
  [_ ^java.io.Writer w]
  (.write w "#T{}"))

;; Public API

(defn treap-set
  "Builds a TreapSet from elems using a random-priority policy, the
  same independent-random-priority assumption the paper's expected
  O(m log(n/m)) union/intersection/difference bounds depend on."
  [& elems]
  (TreapSet. nil (reduce (fn [t k] (impl/-insert t (impl/random-priority k) k k)) EMPTY elems)))
