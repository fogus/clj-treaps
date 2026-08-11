(ns fogus.treaps)

(set! *warn-on-reflection* true)

(declare walk delete-impl uproot-impl split-impl random-priority EMPTY)

(defprotocol RestructuringProtocol
  (-rotate-right [this])
  (-rotate-left  [this])
  (-rotate       [this]))

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

(defn- kompare [l r]
  (let [c (clojure.core/compare (hash l) (hash r))]
    (cond
      (not (zero? c)) c
      (= l r) 0
      :else (clojure.core/compare
             (System/identityHashCode l)
             (System/identityHashCode r)))))

(deftype Treap [p k v L R]
  PriorityProtocol
  (-priority [_] p)
  RestructuringProtocol
  (-rotate-left [_]
    (let [^Treap new-l (if R R (Treap. -1 nil nil nil nil))]
      (Treap. (.p new-l)
              (.k new-l)
              (.v new-l)
              (Treap. p k v L (.L new-l))
              (.R new-l))))
  (-rotate-right [_]
    (let [^Treap new-r (if L L (Treap. -1 nil nil nil nil))]
      (Treap. (.p new-r)
              (.k new-r)
              (.v new-r)
              (.L new-r)
              (Treap. p k v (.R new-r) R))))
  (-rotate [this]
    (cond
      (< p (-priority L)) (-rotate-right this)
      (< p (-priority R)) (-rotate-left this)
      :else this))
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
      EMPTY))
  (-uproot [this]
    (if-let [result (uproot-impl this)]
      result
      EMPTY))

  PersistentTreap
  (-split [this key]
    (split-impl this key))
  (-join [this other]
    (if (> p (-priority other))
      (Treap. p k v L (-join R other))
      (let [^Treap o other]
        (Treap. (.p o) (.k o) (.v o) (-join this (.L o)) (.R o)))))
  (-union [this other]) ;; TODO
  (-intersect [this other]) ;; TODO
  (-diff [this other]) ;; TODO
  
  clojure.lang.Seqable
  (seq [this]
    (walk (fn [^Treap t] (.k t)) seq this))

  java.lang.Object
  (equals [this other]
    (clojure.lang.APersistentSet/setEquals other this))
  ;; TODO cache (with mutable field?)
  (hashCode [this]
    (clojure.lang.Murmur3/hashUnordered this))
  
  clojure.lang.IPersistentCollection
  (cons [this obj]
    (-insert this p obj obj))
  (empty [_] EMPTY)
  (equiv [this other]
    (cond
      (or (instance? clojure.lang.IPersistentSet other) (instance? java.util.Set other))
      (and (= (count this) (count other))
           (every? #(.contains ^java.util.Set other %) (seq this)))
      :else false))

  clojure.lang.IPersistentSet
  (disjoin [this key]
    (let [[L _ R] (-split this key)]
      (-join L R)))
  (get [this key]
    (-find this key))
  
  clojure.lang.Counted
  (count [this]
    (count (seq this)))

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
  (size [this] (count (seq this)))
  (toArray [this] (into-array Object this))
  (^objects toArray [this ^objects arr]) ;; TODO

  java.lang.Iterable
  (iterator [this]
    (clojure.lang.SeqIterator. (seq this)))

  clojure.lang.IHashEq
  (hasheq [this]
    (clojure.lang.Murmur3/hashUnordered this)))

(extend-type nil
  PriorityProtocol
  (-priority [_] Long/MIN_VALUE)
  
  RestructuringProtocol
  (-rotate-right [_] nil)
  (-rotate-left [_] nil)
  (-rotate [_] nil)
  
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
  (-intersect [_ other] other)
  (-diff [_ other] other))

(defn- delete-impl
  [^Treap t key]
  (when-not (nil? t)
    (let [c (kompare key (.k t))]
      (cond
        (neg? c) (Treap. (.p t) (.k t) (.v t) (delete-impl (.L t) key) (.R t))
        (pos? c) (Treap. (.p t) (.k t) (.v t) (.L t) (delete-impl (.R t) key))
        :else (uproot-impl t)))))

(defn- uproot-impl
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
  "Top-down split of node by key. Returns a triple [less eq gtr] of:
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

(deftype EmptyTreap []
  PriorityProtocol
  (-priority [_] Long/MIN_VALUE)

  RestructuringProtocol
  (-rotate-right [this] this)
  (-rotate-left [this] this)
  (-rotate [this] this)

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
  (-union [this other]) ;; TODO
  (-intersect [this other]) ;; TODO
  (-diff [this other]) ;; TODO
  
  clojure.lang.Seqable
  (seq [_] nil)

  java.lang.Object
  (equals [this other] (clojure.lang.APersistentSet/setEquals other this))
  (hashCode [_] 0)

  clojure.lang.IPersistentCollection
  (cons [this obj] (-insert this (random-priority obj) obj obj))
  (empty [this] this)
  (equiv [this other] (.equals this other))

  clojure.lang.IPersistentSet
  (disjoin [this _] this)
  (get [this _] nil)

  clojure.lang.Counted
  (count [_] 0)

  clojure.lang.IFn
  (invoke [this _] nil)
  (invoke [this key not-found] not-found)

  java.util.Set
  (add [_ _] (throw (UnsupportedOperationException.)))
  (addAll [_ _] (throw (UnsupportedOperationException.)))
  (clear [_] (throw (UnsupportedOperationException.)))
  (remove [_ _] (throw (UnsupportedOperationException.)))
  (removeAll [_ _] (throw (UnsupportedOperationException.)))
  (retainAll [_ _] (throw (UnsupportedOperationException.)))
  (contains [_ _] false)
  (containsAll [_ coll] false)
  (isEmpty [_] true)
  (size [_] 0)
  (toArray [_] (into-array Object []))
  (^objects toArray [_ ^objects arr] arr)

  java.lang.Iterable
  (iterator [_] (clojure.lang.SeqIterator. nil))

  clojure.lang.IHashEq
  (hasheq [_] (clojure.lang.Murmur3/hashUnordered nil)))

(def ^:private EMPTY (EmptyTreap.))

(defn- walk [elem-fn build-fn trp]
  (letfn [(visit [^Treap t]
            (when-not (nil? t)
              (concat (visit (.L t)) [(elem-fn t)] (visit (.R t)))))]
    (build-fn (visit trp))))

(defn- random-priority [_]
  (rand-int Integer/MAX_VALUE))

(defn treap-with-priority-fn
  ""
  [priority-fn & elems]
  (reduce (fn [t k] (-insert t (priority-fn k) k k))
          EMPTY
          elems))

(defn treap
  "Builds a Treap from elems using a random-priority policy."
  [& elems]
  (apply treap-with-priority-fn random-priority elems))

(defmethod print-method Treap
  [^Treap t ^java.io.Writer w]
  (.write w (str "#T{"))  (print-method (.p t) w)
  (.write w (str " ("))   (print-method (.k t) w)
  (.write w (str " => ")) (print-method (.v t) w)
  (.write w (str ")"))   
  (.write w (str " "))   (print-method (.L t) w)
  (.write w (str " "))   (print-method (.R t) w)
  (.write w "}"))

(defmethod print-method EmptyTreap
  [_ ^java.io.Writer w]
  (.write w "#T{}"))

(comment

  (def s #{1 2 3 4 5 :a})
  (def t (treap 1 2 3 4 5 :a))

  (iterator-seq (.iterator t))
  [(hash t) (hash s)]

  (count t)

  (cons :b s)
  (cons :b t)

  (treap)
  (count (Treap. (random-priority nil) nil nil nil nil))

  (.contains t 3)
  (.contains t :a)
  (.contains t 77)

  (.containsAll t [1 2 3 :a])
  (.containsAll t [1 2 3 :b])

  (get s 3)
  (get s 77)
  (get t 3)
  (get t 77)
  (get t 77 :nf)

  (t 3)
  (t 77)
  (t 77 :nf)

  (.equals t s)
  (.equals (cons :b t) s)
  (.equals s t)

  (str t)
)
