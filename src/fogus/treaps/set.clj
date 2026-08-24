(ns fogus.treaps.set
  (:require [fogus.treaps :as treaps]
            [fogus.treaps.impl :as impl]))

(defn- ensure-treap-set
  [maybe-ts]
  (if (instance? fogus.treaps.TreapSet maybe-ts)
    maybe-ts
    (fogus.treaps.TreapSet.
     (or (impl/-derive-root maybe-ts) treaps/EMPTY)
     impl/random-priority)))

(defn union
  "Set union."
  ([] (treaps/treap-set))
  ([s] (ensure-treap-set s))
  ([s1 s2] (ensure-treap-set (impl/-union s1 s2)))
  ([s1 s2 & more] (reduce union (union s1 s2) more)))

(defn intersection
  "Set intersection."
  ([s] (ensure-treap-set s))
  ([s1 s2] (ensure-treap-set (impl/-intersect s1 s2)))
  ([s1 s2 & more] (reduce intersection (intersection s1 s2) more)))

(defn difference
  "Set difference."
  ([s] (ensure-treap-set s))
  ([s1 s2] (ensure-treap-set (impl/-diff s1 s2)))
  ([s1 s2 & more] (reduce difference (difference s1 s2) more)))
