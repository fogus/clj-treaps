(ns clj-treaps.test.treaps-test
  (:require [fogus.treaps :as treap])
  (:use clojure.test)
  (:import [fogus.treaps Treap]))

(set! *warn-on-reflection* true)

(defn ^:private check-left-rotation
  [^Treap orig ^Treap rotated]
  (are [x y] (= x y)
       ;; the treap in orig's R is now the root       
       (.p rotated) (.p ^Treap (.R orig))
       (.k rotated) (.k ^Treap (.R orig))
       (.v rotated) (.v ^Treap (.R orig))
       ;; orig is in the L of rotated       
       (.p orig)    (.p ^Treap (.L rotated))
       (.k orig)    (.k ^Treap (.L rotated))
       (.v orig)    (.v ^Treap (.L rotated))
       ;; rotated's R is it's old R       
       (.p ^Treap (.R rotated)) (.p ^Treap (.R ^Treap (.R orig)))
       (.k ^Treap (.R rotated)) (.k ^Treap (.R ^Treap (.R orig)))
       (.v ^Treap (.R rotated)) (.v ^Treap (.R ^Treap (.R orig)))
       ;; rotated embedded orig's R is orig's old R's L
       (.p ^Treap (.R ^Treap (.L rotated))) (.p ^Treap (.L ^Treap (.R orig)))
       (.k ^Treap (.R ^Treap (.L rotated))) (.k ^Treap (.L ^Treap (.R orig)))
       (.v ^Treap (.R ^Treap (.L rotated))) (.v ^Treap (.L ^Treap (.R orig)))))

(defn ^:private check-right-rotation
  [^Treap orig ^Treap rotated]
  (are [x y] (= x y)
       ;; the treap in orig's L is now the root
       (.p rotated) (.p ^Treap (.L orig))
       (.k rotated) (.k ^Treap (.L orig))
       (.v rotated) (.v ^Treap (.L orig))
       ;; orig is in the R of rotated
       (.p orig)    (.p ^Treap (.R rotated))
       (.k orig)    (.k ^Treap (.R rotated))
       (.v orig)    (.v ^Treap (.R rotated))
       ;; rotated's L is it's old L
       (.p ^Treap (.L rotated)) (.p ^Treap (.L ^Treap (.L orig)))
       (.k ^Treap (.L rotated)) (.k ^Treap (.L ^Treap (.L orig)))
       (.v ^Treap (.L rotated)) (.v ^Treap (.L ^Treap (.L orig)))
       ;; rotated embedded orig's R is orig's old R's L
       (.p ^Treap (.L ^Treap (.R rotated))) (.p ^Treap (.R ^Treap (.L orig)))
       (.k ^Treap (.L ^Treap (.R rotated))) (.k ^Treap (.R ^Treap (.L orig)))
       (.v ^Treap (.L ^Treap (.R rotated))) (.v ^Treap (.R ^Treap (.L orig)))))

(def ^:private a-treap
  (Treap. 1 :a 0 
          (Treap. 2 :b 9 
                  (Treap. 3 :c 36 nil nil)
                  (Treap. 4 :d 42 nil nil))
          (Treap. 5 :e 108
                  (Treap. 6 :f 138 nil nil)
                  (Treap. 7 :g 144 nil nil))))

(deftest test-rotate-left
  (let [^Treap t (Treap. 1 :a 42 nil nil)
        ^Treap r (treap/-rotate-left t)
        ^Treap tgt (.L r)]
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
  (let [^Treap t (Treap. 1 :a 42 nil nil)
        ^Treap r (treap/-rotate-right t)
        ^Treap tgt (.R r)]
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
       nil (treap/-delete nil 0)
       nil (treap/-uproot nil)))

(defn- order-priorites [t]
  (#'fogus.treaps/walk #(.p ^Treap %) identity t))

(deftest test-treap-construction
  (let [t5 (treap/treap 1 2 3 4 5)]
    (is (= 5 (count (seq t5))))
    (is (= #{1 2 3 4 5} (set (seq t5)))))
  (let [t6 (treap/treap :a 1 :b 2 :c 3)]
    (is (= 6 (count (seq t6)))))
  (let [ks [10 3 7 1 9 4 6 2 8 5]
        t1 (apply treap/treap ks)
        t2 (apply treap/treap ks)]
    (is (not= (order-priorites t1) (order-priorites t2))))
  (let [ks [10 3 7 1 9 4 6 2 8 5]
        h1 (apply treap/treap-with-priority-fn hash ks)
        h2 (apply treap/treap-with-priority-fn hash ks)]
    (is (= 3 (treap/-find h1 3)))
    (is (= (order-priorites h1) (order-priorites h2))))
  (testing "that hash collision handled properly"
    (let [a (reify Object
              (hashCode [_] 42)
              (equals [this o] (identical? this o))
              (toString [_] "a"))
          b (reify Object
              (hashCode [_] 42)
              (equals [this o] (identical? this o))
              (toString [_] "b"))
          t (-> nil
                (treap/-insert 1 a :a)
                (treap/-insert 2 b :b))]
      (is (= 2 (count (seq t))))
      (is (= :a (treap/-find t a)))
      (is (= :b (treap/-find t b))))))

(deftest test-delete
  (testing "delete-then-find returns nil"
    (let [t0 (treap/treap 1 2 3 4 5)
          t1 (treap/-delete t0 3)]
      (is (nil? (treap/-find t1 3)))
      (is (= #{1 2 4 5} (set (seq t1))))))

  (testing "deleting each key of a small treap in turn leaves the rest intact"
    (let [ks [10 3 7 1 9 4 6 2 8 5]
          t0 (apply treap/treap ks)]
      (doseq [k ks]
        (let [t1 (treap/-delete t0 k)]
          (is (nil? (treap/-find t1 k)))
          (is (= (disj (set ks) k) (set (seq t1))))))))

  (testing "deleting an absent key is a no-op"
    (let [t0 (treap/treap 1 2 3)
          t1 (treap/-delete t0 999)]
      (is (= (set (seq t0)) (set (seq t1))))
      (is (= (order-priorites t0) (order-priorites t1)))))

  (testing "inserting then deleting every key of a randomized tree empties it"
    (let [elems (shuffle (range 50))
          t (apply treap/treap elems)
          emptied (reduce (fn [acc k] (treap/-delete acc k)) t (shuffle elems))]
      (is (nil? (seq emptied))))))

(deftest test-split
  (testing "split on nil"
    (let [[l eq g] (treap/-split nil :foo)]
      (is (nil? eq))
      (is (nil? (seq l)))
      (is (nil? (seq g)))))
  (testing "putting a split back together"
    (let [^Treap t  (treap/treap 1 2 3 :a :b {} 1 -42 [])
          [l ^Treap eq g] (treap/-split t :a)]
      (is (= (set (seq t)) (set (seq (into (conj g (.k eq)) l)))))))) 

(deftest test-join
  (testing "join with empty is identity"
    (let [ks (range 300 310)
          t (apply treap/treap ks)
          min-key (first ks)]
      (is (identical? t (treap/-join t nil)))
      (is (identical? t (treap/-join nil t)))
      (is (nil? (treap/-join nil nil)))))
  (testing "reconstituting a split w/o the key"
    (let [t (apply treap/treap (range -15 16))
          [l eq g] (treap/-split t key)
          joined (treap/-join l g)]
      (is (not (some #{eq} (seq joined)))))))

(deftest test-iperset
  (let [s #{1 2 3 4 5 :a}
        t (apply treap/treap s)]
    (is (= s t)))

  (testing "contains? and get"
    (let [s #{:a :b :c 1 2 3 {} [] -777 1.4545 1/7}
          t (apply treap/treap s)]
      (doseq [x s]
        (is (contains? t x))
        (is (= x (get t x))))))

  (testing "disj"
    (let [^Treap t0 (treap/treap 1 2 3 4 5)
          ^Treap t1 (disj t0 3)]
      (is (not (nil? t1)))
      (is (instance? Treap t1))
      (is (not= (-> t1 seq set) (-> t0 seq set)))
      (is (not (contains? t1 3))))
    (let [^Treap t0 (treap/treap 1 2 3)
          ^Treap t1 (disj t0 :not-here)]
      (is (= (-> t1 seq set) (-> t0 seq set))))))

(deftest test-object
  (let [^Treap t1 (treap/treap 1 2 3)
        ^Treap t2 (treap/treap 3 2 1)
        ^Treap t3 (treap/treap 1 2 4)
        s #{1 2 3}]
    (testing "hashCode"
      (is (= (.hashCode t1) (.hasheq t1)))
      (is (= (.hashCode t1) (.hashCode t2)))
      (is (= (hash t1) (hash s))))
    (testing "equiv"
      (is (.equiv t1 t2))
      (is (= t1 t2))
      (is (.equiv t1 s))
      (is (= t1 s))
      (is (not (.equiv t1 t3)))
      (is (not (.equiv t1 [1 2 3])))
      (is (not (.equiv t1 '(1 2 3)))))
    (testing "hashCode/equiv"
      (is (contains? #{t1} t2))
      (is (= :found (get {t1 :found} t2)))
      (is (not (contains? #{t1} t3)))
      (is (nil? (get {t1 :found} t3)))
      (is (= #{t1} #{t2})))))
