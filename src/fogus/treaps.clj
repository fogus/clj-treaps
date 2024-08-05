(ns fogus.treaps)

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
  (-update [this f v])
  (-insert [this v])
  (-delete [this k])
  (-uproot [this]))

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
  (-update [_ k v] nil)
  (-insert [_ _] nil)
  (-delete [_ _] nil)
  (-uproot [_] nil))

(deftype Treap [p k v L R]
  PriorityProtocol
  (-priority [_] p)
  RestructuringProtocol
  (-rotate-left [_]
    (let [new-l (if R R (Treap. -1 nil nil nil nil))]
      (Treap. (.p new-l)
              (.k new-l)
              (.v new-l)
              (Treap. p k v L (.L new-l))
              (.R new-l))))
  (-rotate-right [_]
    (let [new-r (if L L (Treap. -1 nil nil nil nil))]
      (Treap. (.p new-r)
              (.k new-r)
              (.v new-r)
              (.L new-r)
              (Treap. p k v (.R new-r) R))))
  (-rotate [this]
    (cond
     (< p (.p L)) (-rotate-right this)
     (< p (.p R)) (-rotate-left this)
     :else this))
  FindProtocol
  (-find [this key]
    (cond
     (< key k) (-find L key)
     (> key k) (-find R key)
     :else v))
  ModificationProtocol
  (-update [this k v]
    (cond
      (< key k) (Treap. p k v (-update k v L) R)
      (> key k) 
      :else ))
  (-insert [this v])
  (-delete [this k])
  (-uproot [this]))

(defn treap [& elems]
  (Treap. 0 :a 1 nil nil))

(defmethod print-method Treap
  [t w]
  (.write w (str "#T{"))  (print-method (.p t) w)
  (.write w (str " ("))   (print-method (.k t) w)
  (.write w (str " => ")) (print-method (.v t) w)
  (.write w (str ")"))   
  (.write w (str " "))   (print-method (.L t) w)
  (.write w (str " "))   (print-method (.R t) w)
  (.write w "}"))



