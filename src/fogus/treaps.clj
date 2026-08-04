(ns fogus.treaps)

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
    (cond
     (< key k) (-find L key)
     (> key k) (-find R key)
     :else v))
  ModificationProtocol
  (-update [this key val]
    (cond
      (< key k) (Treap. p k v (-update L key val) R)
      (> key k) (Treap. p k v L (-update R key val))
      :else (Treap. p k val L R)))
  (-insert [this pri key val]
    (cond
      (< key k) (let [trp (Treap. p k v (-insert L pri key val) R)]
                  (if (> (-priority (.L ^Treap trp)) (.p ^Treap trp))
                    (-rotate-right trp)
                    trp))
      (> key k) (let [trp (Treap. p k v L (-insert R pri key val))]
                  (if (> (-priority (.R ^Treap trp)) (.p ^Treap trp))
                    (-rotate-left trp)
                    trp))
      :else this))
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
  (-update [_ _ _] nil)
  (-insert [_ p k v] (Treap. p k v nil nil))
  (-delete [_ _] nil)
  (-uproot [_] nil))

(defn treap [& elems]
  (Treap. 0 :a 1 nil nil))

(defmethod print-method Treap
  [^Treap t ^java.io.Writer w]
  (.write w (str "#T{"))  (print-method (.p t) w)
  (.write w (str " ("))   (print-method (.k t) w)
  (.write w (str " => ")) (print-method (.v t) w)
  (.write w (str ")"))   
  (.write w (str " "))   (print-method (.L t) w)
  (.write w (str " "))   (print-method (.R t) w)
  (.write w "}"))



