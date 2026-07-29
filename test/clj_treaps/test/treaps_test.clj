(ns clj-treaps.test.treaps-test
  (:require [fogus.treaps :as treap])
  (:use [clojure.test :only (deftest are)])
  (:import [fogus.treaps Treap]))

(defn ^:private check-left-rotation
  [orig rotated]
  (are [x y] (= x y)
       ;; the treap in orig's R is now the root       
       (.p rotated) (.p (.R orig))
       (.k rotated) (.k (.R orig))
       (.v rotated) (.v (.R orig))
       ;; orig is in the L of rotated       
       (.p orig)    (.p (.L rotated))
       (.k orig)    (.k (.L rotated))
       (.v orig)    (.v (.L rotated))
       ;; rotated's R is it's old R       
       (.p (.R rotated)) (.p (.R (.R orig)))
       (.k (.R rotated)) (.k (.R (.R orig)))
       (.v (.R rotated)) (.v (.R (.R orig)))
       ;; rotated embedded orig's R is orig's old R's L
       (.p (.R (.L rotated))) (.p (.L (.R orig)))
       (.k (.R (.L rotated))) (.k (.L (.R orig)))
       (.v (.R (.L rotated))) (.v (.L (.R orig)))))

(defn ^:private check-right-rotation
  [orig rotated]
  (are [x y] (= x y)
       ;; the treap in orig's L is now the root
       (.p rotated) (.p (.L orig))
       (.k rotated) (.k (.L orig))
       (.v rotated) (.v (.L orig))
       ;; orig is in the R of rotated
       (.p orig)    (.p (.R rotated))
       (.k orig)    (.k (.R rotated))
       (.v orig)    (.v (.R rotated))
       ;; rotated's L is it's old L
       (.p (.L rotated)) (.p (.L (.L orig)))
       (.k (.L rotated)) (.k (.L (.L orig)))
       (.v (.L rotated)) (.v (.L (.L orig)))
       ;; rotated embedded orig's R is orig's old R's L
       (.p (.L (.R rotated))) (.p (.R (.L orig)))
       (.k (.L (.R rotated))) (.k (.R (.L orig)))
       (.v (.L (.R rotated))) (.v (.R (.L orig)))
       ))

(def ^:private a-treap
  (Treap. 1 :a 0 
          (Treap. 2 :b 9 
                  (Treap. 3 :c 36 nil nil)
                  (Treap. 4 :d 42 nil nil))
          (Treap. 5 :e 108
                  (Treap. 6 :f 138 nil nil)
                  (Treap. 7 :g 144 nil nil))))

(deftest test-rotate-left
  (let [t (Treap. 1 :a 42 nil nil)
        r (treap/-rotate-left t)
        tgt (.L r)]
    (are [x y] (= x y)
         (.p r) -1
         (.k r) nil
         (.v r) nil
         (.p tgt) 1
         (.k tgt) :a
         (.v tgt) 42
         (.L tgt) nil
         (.R tgt) nil
         (.R r) nil))
  (check-left-rotation a-treap (treap/-rotate-left a-treap)))

(deftest test-rotate-right
  (let [t (Treap. 1 :a 42 nil nil)
        r (treap/-rotate-right t)
        tgt (.R r)]
    (are [x y] (= x y)
         (.p r) -1
         (.k r) nil
         (.v r) nil
         (.p tgt) 1
         (.k tgt) :a
         (.v tgt) 42
         (.R tgt) nil
         (.L tgt) nil
         (.L r) nil))
  (check-right-rotation a-treap (treap/-rotate-right a-treap)))

(deftest test-nil
  (are [x y] (= x y)
       -1  (treap/-priority nil)
       nil (treap/-rotate nil)
       nil (treap/-rotate-right nil)
       nil (treap/-rotate-left  nil)
       nil (treap/-find nil 0)
;;       nil (treap/-insert nil 0 1 2)
       nil (treap/-delete nil 0)
       nil (treap/-uproot nil)))

(deftest test-priority
  (are [x y] (= x y)
       42 (treap/-priority (Treap. 42 nil nil nil nil))))

