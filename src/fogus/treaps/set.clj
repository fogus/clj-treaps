(ns fogus.treaps.set
  (:require [fogus.treaps :as treaps]
            [fogus.treaps.impl :as impl]))

(set! *warn-on-reflection* true)

(defn- ensure-treap-set
  [maybe-ts]
  (if (instance? fogus.treaps.TreapSet maybe-ts)
    maybe-ts
    (fogus.treaps.TreapSet. nil (or (impl/-derive-root maybe-ts) impl/EMPTY_TREAP) 0 0)))

(defn union
  "Return a treap set that is the union of the input treap sets."
  ([] (treaps/treap-set))
  ([t] (ensure-treap-set t))
  ([t1 t2] (ensure-treap-set (impl/-union t1 t2)))
  ([t1 t2 & more] (reduce union (union t1 t2) more)))

(defn intersection
  "Return a treap set that is the intersection of the input treap sets."
  ([t] (ensure-treap-set t))
  ([t1 t2] (ensure-treap-set (impl/-intersect t1 t2)))
  ([t1 t2 & more] (reduce intersection (intersection t1 t2) more)))

(defn difference
  "Return a treap set that is the first set without elements of the remaining treap sets"
  ([t] (ensure-treap-set t))
  ([t1 t2] (ensure-treap-set (impl/-diff t1 t2)))
  ([t1 t2 & more] (reduce difference (difference t1 t2) more)))

(defn subset?
  "Is t1 a subset of t2?"
  [t1 t2]
  (empty? (impl/-diff t1 t2)))

(defn superset?
  "Is t1 a superset of t2?"
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

(defn intersects?
  "Whether t1 and t2 have any elements in common."
  [t1 t2]
  (boolean (some #(contains? t2 %) t1)))

(defn disjoint?
  "Whether t1 and t2 have no elements in common."
  [t1 t2]
  (not (intersects? t1 t2)))
