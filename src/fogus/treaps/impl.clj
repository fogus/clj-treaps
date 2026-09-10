(ns ^:no-doc fogus.treaps.impl
  "Kernel implementation of the treap described in \"Fast Set
  Operations Using Treaps\" by Guy E. Blelloch and Margaret
  Reid-Miller.")

(set! *warn-on-reflection* true)

(declare delete-impl uproot-impl split-impl diff-impl EMPTY_TREAP)

(defn random-priority
  "The balance invariant for treaps depends on priorities being
  independent and random. This allows the treap to avoid rebalancing
  since randomness balances the tree in-place, regardless of insertion
  order or key values."
  [_]
  (rand-int Integer/MAX_VALUE))

(defn kompare
  "A total order over arbitrary Clojure values, since treaps need one
  but Clojure values don't have one in general. Orders by hash, and
  breaks hash ties between distinct values using identity hash."
  [l r]
  (let [c (clojure.core/compare (hash l) (hash r))]
    (cond
      (not (zero? c)) c
      (= l r) 0
      :else (clojure.core/compare
             (System/identityHashCode l)
             (System/identityHashCode r)))))

(defprotocol RestructuringProtocol
  (-rotate-right [this])
  (-rotate-left  [this]))

(defprotocol PriorityProtocol
  (-priority [this]))

(defprotocol FindProtocol
  (-find [this v]))

(defprotocol ModificationProtocol
  (-update [this k v])
  (-insert [this p k v])
  (-delete [this k])
  (-uproot [this]))

(defprotocol PersistentTreap
  (-split [this key])
  (-join [this other])
  (-union [this other])
  (-intersect [this other])
  (-diff [this other]))

(defprotocol TreapRoot
  (-derive-root [this]))

(defprotocol TreapSPI
  "The seam that enables the Treap struct to back more than one
  collection type. Treap is deliberately agnostic about what k and v
  mean to a concrete collection. The paper only needs k for ordering
  and never inspects v at all, so adding a semantic shim is done here."
  (-materialize-kv [provider k v]
    "Turns a node's k/v pair into the value the collection exposes to
    the outside world when walking the tree.")
  (-unwrap-kv [provider obj]
    "Unpacks obj into the [k v] pair the kernel's -insert needs to build
    or update a node."))

(deftype Treap [p k v L R]
  PriorityProtocol
  (-priority [_] p)
  RestructuringProtocol
  ;; Promotes this node's right child to the root of the subtree while
  ;; leaving in-order key order unchanged. The paper describes join as
  ;; interleaving pieces of one treap's right spine and the other's left
  ;; spine so priorities descend all the way to a leaf.
  (-rotate-left [this]
    (if (nil? R)
      this
      (let [^Treap r R]
        (Treap. (.p r) (.k r) (.v r)
                (Treap. p k v L (.L r))
                (.R r)))))
  ;; Mirrors -rotate-left
  (-rotate-right [this]
    (if (nil? L)
      this
      (let [^Treap l L]
        (Treap. (.p l) (.k l) (.v l)
                (.L l)
                (Treap. p k v (.R l) R)))))
  FindProtocol
  (-find [this key]
    (let [c (kompare key k)]
      (cond
        (neg? c) (-find L key)
        (pos? c) (-find R key)
        :else v)))
  ModificationProtocol
  (-update [this key val]
    (let [c (kompare key k)]
      (cond
        (neg? c) (Treap. p k v (-update L key val) R)
        (pos? c) (Treap. p k v L (-update R key val))
        :else (Treap. p k val L R))))
  ;; Inserts via recursion on key, then restores heap order coming back
  ;; up the call stack: whenever the freshly-rebuilt child now outranks
  ;; this node's own priority, rotate that child into this node's place.
  (-insert [this pri key val]
    (let [c (kompare key k)]
      (cond
        (neg? c) (let [trp (Treap. p k v (-insert L pri key val) R)]
                    (if (> (-priority (.L ^Treap trp)) (.p ^Treap trp))
                      (-rotate-right trp)
                      trp))
        (pos? c) (let [trp (Treap. p k v L (-insert R pri key val))]
                    (if (> (-priority (.R ^Treap trp)) (.p ^Treap trp))
                      (-rotate-left trp)
                      trp))
        :else this)))
  (-delete [this key]
    (if-let [result (delete-impl this key)]
      result
      EMPTY_TREAP))
  (-uproot [this]
    (if-let [result (uproot-impl this)]
      result
      EMPTY_TREAP))

  PersistentTreap
  (-split [this key] (split-impl this key))
  ;; Joins two treaps where every key here is less than every key in
  ;; `other`, per the paper's join: whichever treap has the higher
  ;; priority becomes the root, and the other treap is joined
  ;; recursively into the child that faces it, so join walks this
  ;; treap's right spine and other's left spine, interleaving them by
  ;; priority until heap order holds all the way to a leaf.
  (-join [this other]
    (if (>= p (-priority other))
      (Treap. p k v L (-join R other))
      (let [^Treap o other]
        (Treap. (.p o) (.k o) (.v o) (-join this (.L o)) (.R o)))))
  ;; Pick whichever treap has the higher priority as the pivot, split
  ;; the other treap by the pivot's key, discard any duplicate node
  ;; the split turns up, and recurse the same union on the
  ;; corresponding less-than and greater-than halves before rejoining
  ;; them under the pivot.
  (-union [this other]
    (cond
      (empty? other) this
      (< (-priority this) (-priority other)) (-union other this)
      :else
      (let [^Treap t this
            [l _ g] (-split other (.k t))]
        (Treap. (.p t) (.k t) (.v t) (-union (.L t) l) (-union (.R t) g)))))
  ;; The pivot key is only kept in the result when the split found a
  ;; matching key in `other`. Otherwise the pivot is dropped and the two
  ;; recursive intersections are joined directly.
  (-intersect [this other]
    (cond
      (empty? other) nil
      (< (-priority this) (-priority other)) (-intersect other this)
      :else
      (let [^Treap t this
            [l dup g] (-split other (.k t))
            left (-intersect (.L t) l)
            right (-intersect (.R t) g)]
        (if (nil? dup)
          (-join left right)
          (Treap. (.p t) (.k t) (.v t) left right)))))
  (-diff [this other] (diff-impl this other true))

  TreapRoot
  (-derive-root [this] this)

  clojure.lang.Counted
  (count [_]
    (+ 1 (count L) (count R)))

  clojure.lang.IFn
  (invoke [this key]
    (-find this key)))

(extend-type nil
  PriorityProtocol
  (-priority [_] Long/MIN_VALUE)

  RestructuringProtocol
  (-rotate-right [_] nil)
  (-rotate-left [_] nil)

  FindProtocol
  (-find [_ _] nil)

  ModificationProtocol
  (-update [_ _ _] nil)
  (-insert [_ p k v] (Treap. p k v nil nil))
  (-delete [_ _] nil)
  (-uproot [_] nil)

  PersistentTreap
  (-split [_ _] [nil nil nil])
  (-join [_ other] other)
  (-union [_ other] other)
  (-intersect [_ _] nil)
  (-diff [_ _] nil)

  TreapRoot
  (-derive-root [_] nil))

(defn- delete-impl
  "Walks down to the node keyed `key` by ordinary BST recursion,
  rebuilding each ancestor on the way, then hands the matched node to
  uproot-impl to remove it while restoring heap order."
  [^Treap t key]
  (when-not (nil? t)
    (let [c (kompare key (.k t))]
      (cond
        (neg? c) (Treap. (.p t) (.k t) (.v t) (delete-impl (.L t) key) (.R t))
        (pos? c) (Treap. (.p t) (.k t) (.v t) (.L t) (delete-impl (.R t) key))
        :else (uproot-impl t)))))

(defn- uproot-impl
  "Removes a node from the middle of a treap by repeatedly rotating
  up whichever child has the higher priority, until the node has at
  most one child and can simply be spliced out. This keeps both heap
  order and BST order intact throughout."
  [^Treap t]
  (cond
    (and (nil? (.L t)) (nil? (.R t))) nil
    (> (-priority (.L t)) (-priority (.R t)))
    (let [^Treap rotated (-rotate-right t)]
      (Treap. (.p rotated) (.k rotated) (.v rotated)
              (.L rotated)
              (uproot-impl (.R rotated))))
    :else
    (let [^Treap rotated (-rotate-left t)]
      (Treap. (.p rotated) (.k rotated) (.v rotated)
              (uproot-impl (.L rotated))
              (.R rotated)))))

(defn- split-impl
  "At each step, compare the current node's key to the target key and,
  depending on which side it falls, either fold the node into the growing
  less-than tree and recurse into its right child, or fold it into the
  growing greater-than tree and recurse into its left child, until a key
  match or an empty subtree is reached. Returns a triple
  [less eq gtr] of:
    less - node whose keys are all < key
    eq   - the node keyed `key`, or nil if absent
    gtr  - node whose keys are all > key"
  [^Treap node key]
  (let [c (kompare (.k node) key)]
    (cond
      (neg? c) (let [[l eq g] (-split (.R node) key)]
                 [(Treap. (.p node) (.k node) (.v node) (.L node) l) eq g])

      (pos? c) (let [[l eq g] (-split (.L node) key)]
                 [l eq (Treap. (.p node) (.k node) (.v node) g (.R node))])

      :else [(.L node) (Treap. (.p node) (.k node) (.v node) nil nil) (.R node)])))

(defn- diff-impl
  "Pivot on whichever of r1/r2 has the higher priority as usual, but
  flip every time the pivot role swaps. A node from the non-subtrahend
  side is kept in the result only when the split found no matching key
  in the subtrahend side; otherwise the two recursive results are joined
  directly and the node is dropped."
  [r1 r2 r2-subtrahend?]
  (cond
    (or (empty? r1) (empty? r2))
    (if r2-subtrahend? r1 r2)

    (< (-priority r1) (-priority r2))
    (diff-impl r2 r1 (not r2-subtrahend?))

    :else
    (let [^Treap t1 r1
          [l dup g] (-split r2 (.k t1))
          left  (diff-impl (.L t1) l r2-subtrahend?)
          right (diff-impl (.R t1) g r2-subtrahend?)]
      (if (and (nil? dup) r2-subtrahend?)
        (Treap. (.p t1) (.k t1) (.v t1) left right)
        (-join left right)))))

(deftype EmptyTreap []
  PriorityProtocol
  (-priority [_] Long/MIN_VALUE)

  RestructuringProtocol
  (-rotate-right [this] this)
  (-rotate-left [this] this)

  FindProtocol
  (-find [_ _] nil)

  ModificationProtocol
  (-update [this _ _] this)
  (-insert [_ p k v] (Treap. p k v nil nil))
  (-delete [this _] this)
  (-uproot [this] this)

  PersistentTreap
  (-split [this _] [this nil this])
  (-join [_ other] other)
  (-union [_ other] other)
  (-intersect [this _] this)
  (-diff [this _] this)

  TreapRoot
  (-derive-root [this] this)

  clojure.lang.Counted
  (count [_] 0)

  clojure.lang.IFn
  (invoke [_ _] nil))

(def EMPTY_TREAP (EmptyTreap.))

(defn walk
  "In-order sequence, run through `build-fn`, materializing k & v per node."
  [spi build-fn trp]
  (when-not (empty? trp)
    (letfn [(visit [^Treap t]
              (when-not (empty? t)
                (concat (visit (.L t))
                        [(-materialize-kv spi (.k t) (.v t))]
                        (visit (.R t)))))]
      (build-fn (visit trp)))))

(defn treap
  "Builds a Treap from elems using a random-priority policy."
  [& elems]
  (reduce (fn [t k] (-insert t (random-priority k) k k))
          EMPTY_TREAP
          elems))
