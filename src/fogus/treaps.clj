(ns fogus.treaps)

(set! *warn-on-reflection* true)

(declare walk)

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
  (-delete [this k]) ;; TODO
  (-uproot [this]) ;; TODO

  clojure.lang.Seqable
  (seq [this]
    (walk (fn [^Treap t] (.k t)) seq this))

  java.lang.Object
  (equals [this other]
    (clojure.lang.APersistentSet/setEquals other this))
  (hashCode [_]) ;; TODO
  
  clojure.lang.IPersistentCollection
  (cons [this obj]
    (-insert this p obj obj))
  (empty [_])  ;; TODO
  (equiv [this other]) ;; TODO

  clojure.lang.IPersistentSet
  (disjoin [_ key]) ;; TODO
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
  (-priority [_] -1)
  
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
  (-uproot [_] nil))

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
          nil
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
