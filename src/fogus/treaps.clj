(ns fogus.treaps
  (:require [clojure.set :as set]
            [fogus.treaps.impl :as impl]))

;; TODO: toArray/1
;; TODO: metadata
;; TODO: cached hashes
;; TODO: docs
;; TODO: tail-recursive split/diff/etc. impls

(set! *warn-on-reflection* true)

(def EMPTY impl/EMPTY_TREAP)

(deftype TreapSet [root priority-fn]
  impl/TreapSPI
  (-materialize-kv [_ k _] k)
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
      (TreapSet. (impl/-insert root (priority-fn k) k v) priority-fn)))
  (empty [_] (TreapSet. EMPTY priority-fn))
  (equiv [this other]
    (cond
      (or (instance? clojure.lang.IPersistentSet other) (instance? java.util.Set other))
      (and (= (count this) (count other))
           (every? #(.contains ^java.util.Set other %) (seq this)))
      :else false))

  clojure.lang.IPersistentSet
  (disjoin [_ key]
    (let [[l _ g] (impl/-split root key)]
      (TreapSet. (impl/-join l g) priority-fn)))
  (get [_ key]
    (impl/-find root key))

  impl/PersistentTreap
  (-split [_ _] (throw (UnsupportedOperationException. "TreapSet does not support -split")))
  (-join [_ _] (throw (UnsupportedOperationException. "TreapSet does not support -join")))
  (-union [_ other]
    (TreapSet. (or (impl/-union root (impl/-derive-root other)) EMPTY) priority-fn))
  (-intersect [_ other]
    (TreapSet. (or (impl/-intersect root (impl/-derive-root other)) EMPTY) priority-fn))
  (-diff [_ other]
    (TreapSet. (or (impl/-diff root (impl/-derive-root other)) EMPTY) priority-fn))

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

(extend-protocol impl/PersistentTreap
  java.util.Set
  (-split [_ _] (throw (UnsupportedOperationException. "java.util.Set does not support -split")))
  (-join [_ _] (throw (UnsupportedOperationException. "java.util.Set does not support -join")))
  (-union [this other] (set/union this other))
  (-intersect [this other] (set/intersection this other))
  (-diff [this other] (set/difference this other)))

(extend-protocol impl/TreapRoot
  java.util.Set
  (-derive-root [this] (apply impl/treap this)))

;; Public API

(defn treap-set-with-priority-fn
  ""
  [priority-fn & elems]
  (TreapSet. (reduce (fn [t k] (impl/-insert t (priority-fn k) k k)) EMPTY elems)
             priority-fn))

(defn treap-set
  "Builds a TreapSet from elems using a random-priority policy."
  [& elems]
  (apply treap-set-with-priority-fn impl/random-priority elems))

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
