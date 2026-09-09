(ns fogus.treaps.set
  (:require [fogus.treaps :as treaps]
            [fogus.treaps.impl :as impl]))

(set! *warn-on-reflection* true)

;; Printing

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
