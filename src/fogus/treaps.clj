(ns fogus.treaps
  (:refer-clojure :exclude [compare]))

(set! *warn-on-reflection* true)

;; PROTOCOLS

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

(declare walk)

(defn- compare [key k]
  (let [c (clojure.core/compare (hash key) (hash k))]
    (cond
      (not (zero? c)) c
      (= key k) 0
      :else (clojure.core/compare (System/identityHashCode key) (System/identityHashCode k)))))

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
    (let [c (compare key k)]
      (cond
        (neg? c) (-find L key)
        (pos? c) (-find R key)
        :else v)))
  ModificationProtocol
  (-update [this key val]
    (let [c (compare key k)]
      (cond
        (neg? c) (Treap. p k v (-update L key val) R)
        (pos? c) (Treap. p k v L (-update R key val))
        :else (Treap. p k val L R))))
  (-insert [this pri key val]
    (let [c (compare key k)]
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
  (-delete [this k])
  (-uproot [this])

  clojure.lang.Seqable
  (seq [this]
    (walk (fn [^Treap t] (.k t)) seq this)))

(defn- walk [elem-fn build-fn trp]
  (letfn [(visit [^Treap t]
            (when-not (nil? t)
              (concat (visit (.L t)) [(elem-fn t)] (visit (.R t)))))]
    (build-fn (visit trp))))

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

(defn treap-with
  ""
  [priority-fn & elems]
  (reduce (fn [t k] (-insert t (priority-fn k) k k))
          nil
          elems))

(defn treap
  "Builds a Treap from elems using a random-priority policy."
  [& elems]
  (apply treap-with #(do % (rand-int Integer/MAX_VALUE)) elems))

(defmethod print-method Treap
  [^Treap t ^java.io.Writer w]
  (.write w (str "#T{"))  (print-method (.p t) w)
  (.write w (str " ("))   (print-method (.k t) w)
  (.write w (str " => ")) (print-method (.v t) w)
  (.write w (str ")"))   
  (.write w (str " "))   (print-method (.L t) w)
  (.write w (str " "))   (print-method (.R t) w)
  (.write w "}"))



