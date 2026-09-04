(ns clj-treaps.test.treaps-test
  (:require [fogus.treaps.impl :as impl])
  (:use clojure.test)
  (:import [fogus.treaps.impl Treap]))

(set! *warn-on-reflection* true)

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
        ^Treap r (impl/-rotate-left t)
        ^Treap orig a-treap
        ^Treap rotated (impl/-rotate-left a-treap)]
    (is (identical? t r))
    (testing "the treap in orig's R is now the root"
      (is (= (.p rotated) (.p ^Treap (.R orig))))
      (is (= (.k rotated) (.k ^Treap (.R orig))))
      (is (= (.v rotated) (.v ^Treap (.R orig)))))
    (testing "orig is in the L of rotated"
      (is (= (.p orig) (.p ^Treap (.L rotated))))
      (is (= (.k orig) (.k ^Treap (.L rotated))))
      (is (= (.v orig) (.v ^Treap (.L rotated)))))
    (testing "rotated's R is it's old R"
      (is (= (.p ^Treap (.R rotated)) (.p ^Treap (.R ^Treap (.R orig)))))
      (is (= (.k ^Treap (.R rotated)) (.k ^Treap (.R ^Treap (.R orig)))))
      (is (= (.v ^Treap (.R rotated)) (.v ^Treap (.R ^Treap (.R orig))))))
    (testing "rotated embedded orig's R is orig's old R's L"
      (is (= (.p ^Treap (.R ^Treap (.L rotated))) (.p ^Treap (.L ^Treap (.R orig)))))
      (is (= (.k ^Treap (.R ^Treap (.L rotated))) (.k ^Treap (.L ^Treap (.R orig)))))
      (is (= (.v ^Treap (.R ^Treap (.L rotated))) (.v ^Treap (.L ^Treap (.R orig)))))))
  (testing "left rotate promotes child"
    (let [^Treap t (Treap. 1 :a 42 nil (Treap. 2 :b 99 nil nil))
          ^Treap r (impl/-rotate-left t)
          ^Treap tgt (.L r)]
      (are [x y] (= x y)
        (.p r) 2
        (.k r) :b
        (.v r) 99
        (.R r) nil
        (.p tgt) 1
        (.k tgt) :a
        (.v tgt) 42
        (.L tgt) nil
        (.R tgt) nil))))

(deftest test-rotate-right
  (let [^Treap t (Treap. 1 :a 42 nil nil)
        ^Treap r (impl/-rotate-right t)
        ^Treap orig a-treap
        ^Treap rotated (impl/-rotate-right a-treap)]
    (is (identical? t r))
    (testing "the treap in orig's L is now the root"
      (is (= (.p rotated) (.p ^Treap (.L orig))))
      (is (= (.k rotated) (.k ^Treap (.L orig))))
      (is (= (.v rotated) (.v ^Treap (.L orig)))))
    (testing "orig is in the R of rotated"
      (is (= (.p orig) (.p ^Treap (.R rotated))))
      (is (= (.k orig) (.k ^Treap (.R rotated))))
      (is (= (.v orig) (.v ^Treap (.R rotated)))))
    (testing "rotated's L is it's old L"
      (is (= (.p ^Treap (.L rotated)) (.p ^Treap (.L ^Treap (.L orig)))))
      (is (= (.k ^Treap (.L rotated)) (.k ^Treap (.L ^Treap (.L orig)))))
      (is (= (.v ^Treap (.L rotated)) (.v ^Treap (.L ^Treap (.L orig))))))
    (testing "rotated embedded orig's R is orig's old R's L"
      (is (= (.p ^Treap (.L ^Treap (.R rotated))) (.p ^Treap (.R ^Treap (.L orig)))))
      (is (= (.k ^Treap (.L ^Treap (.R rotated))) (.k ^Treap (.R ^Treap (.L orig)))))
      (is (= (.v ^Treap (.L ^Treap (.R rotated))) (.v ^Treap (.R ^Treap (.L orig)))))))
  (testing "right rotate promotes child"
    (let [^Treap t (Treap. 1 :a 42 (Treap. 2 :b 99 nil nil) nil)
          ^Treap r (impl/-rotate-right t)
          ^Treap tgt (.R r)]
      (are [x y] (= x y)
        (.p r) 2
        (.k r) :b
        (.v r) 99
        (.L r) nil
        (.p tgt) 1
        (.k tgt) :a
        (.v tgt) 42
        (.L tgt) nil
        (.R tgt) nil)))

  (deftest test-nil
    (are [x y] (= x y)
      nil (impl/-rotate-right nil)
      nil (impl/-rotate-left  nil)
      nil (impl/-find nil 0)
      nil (impl/-delete nil 0)
      nil (impl/-uproot nil))))

(deftest test-treap-construction
  (let [t5 (impl/treap 1 2 3 4 5)]
    (is (= 5 (count t5))))
  (let [t6 (impl/treap :a 1 :b 2 :c 3)]
    (is (= 6 (count t6))))
  (let [ks [10 3 7 1 9 4 6 2 8 5]
        t1 (apply impl/treap ks)
        t2 (apply impl/treap ks)])
  (let [ks [10 3 7 1 9 4 6 2 8 5]
        h1 (apply impl/treap-with-priority-fn hash ks)
        h2 (apply impl/treap-with-priority-fn hash ks)]
    (is (= 3 (impl/-find h1 3))))
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
                (impl/-insert 1 a :a)
                (impl/-insert 2 b :b))]
      (is (= 2 (count t)))
      (is (= :a (impl/-find t a)))
      (is (= :b (impl/-find t b))))))

(deftest test-delete
  (testing "delete-then-find returns nil"
    (let [t0 (impl/treap 1 2 3 4 5)
          t1 (impl/-delete t0 3)]
      (is (nil? (impl/-find t1 3)))))

  (testing "deleting each key of a small treap in turn leaves the rest intact"
    (let [ks [10 3 7 1 9 4 6 2 8 5]
          t0 (apply impl/treap ks)]
      (doseq [k ks]
        (let [t1 (impl/-delete t0 k)]
          (is (nil? (impl/-find t1 k)))))))

  (testing "deleting an absent key is a no-op"
    (let [t0 (impl/treap 1 2 3)
          t1 (impl/-delete t0 999)]
      (is (= 1 (impl/-find t1 1)))
      (is (= 2 (impl/-find t1 2)))
      (is (= 3 (impl/-find t1 3)))))

  (testing "inserting then deleting every key of a randomized tree empties it"
    (let [elems (shuffle (range 50))
          t (apply impl/treap elems)
          emptied (reduce (fn [acc k] (impl/-delete acc k)) t (shuffle elems))]
      (is (zero? (count emptied))))))

(deftest test-split
  (testing "split on nil"
    (let [[l eq g] (impl/-split nil :foo)]
      (is (nil? eq))
      (is (nil? (seq l)))
      (is (nil? (seq g))))))

(deftest test-join
  (testing "join with empty is a no-op"
    (let [ks (range 300 310)
          t (apply impl/treap ks)
          min-key (first ks)]
      (is (identical? t (impl/-join nil t)))
      (is (nil? (impl/-join nil nil)))))
  (testing "reconstituting a split w/o the key"
    (let [t (apply impl/treap (range -15 16))
          [l eq g] (impl/-split t key)
          joined (impl/-join l g)]
      (is (= eq (impl/-find eq joined))))))
