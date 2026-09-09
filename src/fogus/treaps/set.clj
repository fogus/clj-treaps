(ns fogus.treaps.set
  (:require [fogus.treaps :as treaps]
            [fogus.treaps.impl :as impl]))

(set! *warn-on-reflection* true)

(defn- ensure-treap-set
  [maybe-ts]
  (if (instance? fogus.treaps.TreapSet maybe-ts)
    maybe-ts
    (fogus.treaps.TreapSet. nil
     (or (impl/-derive-root maybe-ts) treaps/EMPTY))))

(defn union
  "Return a treap set that is the union of the input treap sets."
  ([] (treaps/treap-set))
  ([s] (ensure-treap-set s))
  ([s1 s2] (ensure-treap-set (impl/-union s1 s2)))
  ([s1 s2 & more] (reduce union (union s1 s2) more)))

(defn intersection
  "Return a treap set that is the intersection of the input treap sets."
  ([s] (ensure-treap-set s))
  ([s1 s2] (ensure-treap-set (impl/-intersect s1 s2)))
  ([s1 s2 & more] (reduce intersection (intersection s1 s2) more)))

(defn difference
  "Return a treap set that is the first set without elements of the remaining treap sets"
  ([s] (ensure-treap-set s))
  ([s1 s2] (ensure-treap-set (impl/-diff s1 s2)))
  ([s1 s2 & more] (reduce difference (difference s1 s2) more)))

(defn subset?
  "Is t1 a subset of t2?"
  [t1 t2]
  (empty? (impl/-diff t1 t2)))

(defn superset?
  "Is set1 a superset of set2?"
  [t1 t2]
  (subset? t2 t1))

(defn symmetric-difference
  "Elements in t1 or t2 but not both."
  [t1 t2]
  (ensure-treap-set
   (cond
     (empty? t1) t2
     (empty? t2) t1
     (identical? t1 t2) (impl/-diff t1 t1)
     :else (impl/-union (impl/-diff t1 t2) (impl/-diff t2 t1)))))
