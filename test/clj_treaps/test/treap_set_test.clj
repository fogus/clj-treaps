(ns clj-treaps.test.treap-set-test
  (:require [fogus.treaps :as treaps]
            [fogus.treaps.set :as treaps.set]
            [clojure.set :as s])
  (:use clojure.test)
  (:import [fogus.treaps TreapSet]))

(set! *warn-on-reflection* true)

(deftest test-treap-set-construction
  (let [s5 (treaps/treap-set 1 2 3 4 5)]
    (is (= 5 (count s5)))
    (is (= #{1 2 3 4 5} (set (seq s5)))))
  (let [s6 (treaps/treap-set :a 1 :b 2 :c 3)]
    (is (= 6 (count s6))))
  (testing "that hash collision handled properly"
    (let [a (reify Object
              (hashCode [_] 42)
              (equals [this o] (identical? this o))
              (toString [_] "a"))
          b (reify Object
              (hashCode [_] 42)
              (equals [this o] (identical? this o))
              (toString [_] "b"))
          s (-> (treaps/treap-set)
                (conj a)
                (conj b))]
      (is (= 2 (count s)))
      (is (contains? s a))
      (is (contains? s b)))))

(deftest test-treap-set-disj
  (testing "disj-then-contains? returns false"
    (let [s0 (treaps/treap-set 1 2 3 4 5)
          s1 (disj s0 3)]
      (is (not (contains? s1 3)))
      (is (= #{1 2 4 5} (set (seq s1))))))

  (testing "disjing each key of a small treap-set in turn leaves the rest intact"
    (let [ks [10 3 7 1 9 4 6 2 8 5]
          s0 (apply treaps/treap-set ks)]
      (doseq [k ks]
        (let [s1 (disj s0 k)]
          (is (not (contains? s1 k)))
          (is (= (disj (set ks) k) (set (seq s1))))))))

  (testing "disjing an absent key is a no-op"
    (let [s0 (treaps/treap-set 1 2 3)
          s1 (disj s0 999)]
      (is (= (set (seq s0)) (set (seq s1))))))

  (testing "conj-ing then disj-ing every key of a randomized treap-set empties it"
    (let [elems (shuffle (range 50))
          s (apply treaps/treap-set elems)
          emptied (reduce disj s (shuffle elems))]
      (is (empty? emptied)))))

(deftest test-treap-set-iperset
  (let [s #{1 2 3 4 5 :a}
        ts (apply treaps/treap-set s)]
    (is (= s ts)))

  (testing "contains? and get"
    (let [s #{:a :b :c 1 2 3 {} [] -777 1.4545 1/7}
          ts (apply treaps/treap-set s)]
      (doseq [x s]
        (is (contains? ts x))
        (is (= x (get ts x))))))

  (testing "disj"
    (let [^TreapSet t0 (treaps/treap-set 1 2 3 4 5)
          ^TreapSet t1 (disj t0 3)]
      (is (not (nil? t1)))
      (is (instance? TreapSet t1))
      (is (not= (-> t1 seq set) (-> t0 seq set)))
      (is (not (contains? t1 3))))
    (let [^TreapSet t0 (treaps/treap-set 1 2 3)
          ^TreapSet t1 (disj t0 :not-here)]
      (is (= (-> t1 seq set) (-> t0 seq set))))))

(deftest test-treap-set-empty-result-identity
  (let [EMPTY @#'treaps/EMPTY
        t1 (treaps/treap-set 1 2 3)
        t2 (treaps/treap-set 4 5 6)
        e  (treaps/treap-set)]
    (is (identical? EMPTY (.root ^TreapSet (treaps.set/intersection t1 t2))))
    (is (identical? EMPTY (.root ^TreapSet (treaps.set/intersection e t1))))
    (is (identical? EMPTY (.root ^TreapSet (treaps.set/intersection t1 e))))
    (is (identical? EMPTY (.root ^TreapSet (treaps.set/intersection e e))))
    (is (identical? EMPTY (.root ^TreapSet (treaps.set/difference e t1))))
    (is (identical? EMPTY (.root ^TreapSet (treaps.set/difference e e))))
    (is (identical? EMPTY (.root ^TreapSet (treaps.set/difference t1 t1))))
    (is (identical? EMPTY (.root ^TreapSet (treaps.set/union e e))))
    (is (not (identical? EMPTY (.root ^TreapSet (treaps.set/union t1 e)))))
    (is (instance? TreapSet (treaps.set/intersection t1 t2)))
    (is (instance? TreapSet (treaps.set/difference e t1)))
    (is (instance? TreapSet (treaps.set/union e e)))
    (let [empty-result (treaps.set/intersection t1 t2)]
      (is (instance? TreapSet (treaps.set/union empty-result t1)))
      (is (instance? TreapSet (treaps.set/intersection empty-result t1)))
      (is (instance? TreapSet (treaps.set/difference empty-result t1)))
      (is (= (set (seq t1)) (set (seq (treaps.set/union empty-result t1)))))
      (is (empty? (seq (treaps.set/intersection empty-result t1))))
      (is (empty? (seq (treaps.set/difference empty-result t1)))))
    (testing "that TreapSet always returns"
      (is (instance? TreapSet (treaps.set/union EMPTY)))
      (is (instance? TreapSet (treaps.set/union EMPTY EMPTY)))
      (is (instance? TreapSet (treaps.set/union EMPTY EMPTY EMPTY)))
      (is (instance? TreapSet (treaps.set/intersection EMPTY)))
      (is (instance? TreapSet (treaps.set/intersection EMPTY EMPTY)))
      (is (instance? TreapSet (treaps.set/intersection EMPTY EMPTY EMPTY)))
      (is (instance? TreapSet (treaps.set/difference EMPTY)))
      (is (instance? TreapSet (treaps.set/difference EMPTY EMPTY)))
      (is (instance? TreapSet (treaps.set/difference EMPTY EMPTY EMPTY))))))

(deftest test-treap-set-nil-and-empty-args
  (let [ts (treaps/treap-set 1 2 3)
        EMPTY @#'treaps/EMPTY]
    (testing "TreapSet mixed with nil and EMPTY"
      (is (instance? TreapSet (treaps.set/union ts nil)))
      (is (= #{1 2 3} (treaps.set/union ts nil)))
      (is (instance? TreapSet (treaps.set/union nil ts)))
      (is (= #{1 2 3} (treaps.set/union nil ts)))
      (is (instance? TreapSet (treaps.set/union ts EMPTY)))
      (is (instance? TreapSet (treaps.set/union EMPTY ts)))
      (is (= #{1 2 3} (treaps.set/union EMPTY ts))))))

(deftest test-treap-set-object
  (let [^TreapSet t1 (treaps/treap-set 1 2 3)
        ^TreapSet t2 (treaps/treap-set 3 2 1)
        ^TreapSet t3 (treaps/treap-set 1 2 4)
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

(defn- tr-union [& sets]
  (apply treaps.set/union (map #(apply treaps/treap-set %) sets)))

(deftest test-treap-set-union
  (let [s1 #{1 2 3 4 5}
        s2 #{6 7 8 9 10}
        t1 (apply treaps/treap-set s1)
        t2 (apply treaps/treap-set s2)]
    (is (instance? TreapSet (treaps.set/union t1 t2)))
    (is (= (s/union s1 s2) (set (seq (treaps.set/union t1 t2))))))

  (let [s1 #{1 2 3 4 5}
        s2 #{4 5 6 7 8}
        t1 (apply treaps/treap-set s1)
        t2 (apply treaps/treap-set s2)]
    (is (= (s/union s1 s2) (set (seq (treaps.set/union t1 t2)))))
    (is (= (s/union s2 s1) (set (seq (treaps.set/union t2 t1))))))

  (testing "union with empty/self"
    (let [ks (range 50)
          t (apply treaps/treap-set ks)
          empty-ts (treaps/treap-set)]
      (is (= (set ks) (set (seq (treaps.set/union t empty-ts)))))
      (is (= (set ks) (set (seq (treaps.set/union empty-ts t)))))
      (is (empty? (seq (treaps.set/union empty-ts empty-ts))))
      (is (= (set ks) (set (seq (treaps.set/union t t)))))
      (let [t1 (apply treaps/treap-set (take 10 ks))
            t2 (apply treaps/treap-set (drop 10 ks))]
        (is (= (set (seq (treaps.set/union t1 t2))) (set (seq (treaps.set/union t2 t1))))))))

  (testing "examples from clojure.set"
    (are [x y] (= x y)
      (tr-union) #{}

      ; identity
      (tr-union #{}) #{}
      (tr-union #{1}) #{1}
      (tr-union #{1 2 3}) #{1 2 3}

      ; 2 sets, at least one is empty
      (tr-union #{} #{}) #{}
      (tr-union #{} #{1}) #{1}
      (tr-union #{} #{1 2 3}) #{1 2 3}
      (tr-union #{1} #{}) #{1}
      (tr-union #{1 2 3} #{}) #{1 2 3}

      ; 2 sets
      (tr-union #{1} #{2}) #{1 2}
      (tr-union #{1} #{1 2}) #{1 2}
      (tr-union #{2} #{1 2}) #{1 2}
      (tr-union #{1 2} #{3}) #{1 2 3}
      (tr-union #{1 2} #{2 3}) #{1 2 3}

      ; 3 sets, some are empty
      (tr-union #{} #{} #{}) #{}
      (tr-union #{1} #{} #{}) #{1}
      (tr-union #{} #{1} #{}) #{1}
      (tr-union #{} #{} #{1}) #{1}
      (tr-union #{1 2} #{2 3} #{}) #{1 2 3}

      ; 3 sets
      (tr-union #{1 2} #{3 4} #{5 6}) #{1 2 3 4 5 6}
      (tr-union #{1 2} #{2 3} #{1 3 4}) #{1 2 3 4})))

(defn- tr-intersection [& sets]
  (apply treaps.set/intersection (map #(apply treaps/treap-set %) sets)))

(deftest test-treap-set-intersect
  (let [s1 #{1 2 3 4 5}
        s2 #{6 7 8 9 10}
        t1 (apply treaps/treap-set s1)
        t2 (apply treaps/treap-set s2)]
    (is (= (s/intersection s1 s2) (set (seq (treaps.set/intersection t1 t2))))))

  (let [s1 #{1 2 3 4 5}
        s2 #{4 5 6 7 8}
        t1 (apply treaps/treap-set s1)
        t2 (apply treaps/treap-set s2)]
    (is (= (s/intersection s1 s2) (set (seq (treaps.set/intersection t1 t2)))))
    (is (= (s/intersection s2 s1) (set (seq (treaps.set/intersection t2 t1))))))

  (testing "intersect with empty/self"
    (let [ks (range 50)
          t (apply treaps/treap-set ks)
          empty-ts (treaps/treap-set)]
      (is (empty? (seq (treaps.set/intersection t empty-ts))))
      (is (empty? (seq (treaps.set/intersection empty-ts t))))
      (is (empty? (seq (treaps.set/intersection empty-ts empty-ts))))
      (is (= (set ks) (set (seq (treaps.set/intersection t t)))))
      (let [t1 (apply treaps/treap-set (take 10 ks))
            t2 (apply treaps/treap-set (drop 10 ks))]
        (is (= (set (seq (treaps.set/intersection t1 t2))) (set (seq (treaps.set/intersection t2 t1))))))))

  (testing "examples from clojure.set"
    (is (thrown? IllegalArgumentException (tr-intersection)))
  
    (are [x y] (= x y)
      ; identity
      (tr-intersection #{}) #{}
      (tr-intersection #{1}) #{1}
      (tr-intersection #{1 2 3}) #{1 2 3}
      
      ; 2 sets, at least one is empty
      (tr-intersection #{} #{}) #{}
      (tr-intersection #{} #{1}) #{}
      (tr-intersection #{} #{1 2 3}) #{}
      (tr-intersection #{1} #{}) #{}
      (tr-intersection #{1 2 3} #{}) #{}

      ; 2 sets
      (tr-intersection #{1 2} #{1 2}) #{1 2}
      (tr-intersection #{1 2} #{3 4}) #{}
      (tr-intersection #{1 2} #{1}) #{1}
      (tr-intersection #{1 2} #{2}) #{2}
      (tr-intersection #{1 2 4} #{2 3 4 5}) #{2 4}

      ; 3 sets, some are empty
      (tr-intersection #{} #{} #{}) #{}
      (tr-intersection #{1} #{} #{}) #{}
      (tr-intersection #{1} #{1} #{}) #{}
      (tr-intersection #{1} #{} #{1}) #{}
      (tr-intersection #{1 2} #{2 3} #{}) #{}

      ; 3 sets
      (tr-intersection #{1 2} #{2 3} #{5 2}) #{2}
      (tr-intersection #{1 2 3} #{1 3 4} #{1 3}) #{1 3}
      (tr-intersection #{1 2 3} #{3 4 5} #{8 2 3}) #{3})))

(defn- tr-difference [& sets]
  (apply treaps.set/difference (map #(apply treaps/treap-set %) sets)))

(deftest test-treap-set-diff
  (let [s1 #{1 2 3 4 5}
        s2 #{6 7 8 9 10}
        t1 (apply treaps/treap-set s1)
        t2 (apply treaps/treap-set s2)]
    (is (= (s/difference s1 s2) (set (seq (treaps.set/difference t1 t2)))))
    (is (= (s/difference s2 s1) (set (seq (treaps.set/difference t2 t1))))))

  (let [s1 #{1 2 3 4 5}
        s2 #{4 5 6 7 8}
        t1 (apply treaps/treap-set s1)
        t2 (apply treaps/treap-set s2)]
    (is (= (s/difference s1 s2) (set (seq (treaps.set/difference t1 t2)))))
    (is (= (s/difference s2 s1) (set (seq (treaps.set/difference t2 t1))))))

  (testing "diff with empty/self"
    (let [ks (range 50)
          t (apply treaps/treap-set ks)
          empty-ts (treaps/treap-set)]
      (is (= (set ks) (set (seq (treaps.set/difference t empty-ts)))))
      (is (empty? (seq (treaps.set/difference empty-ts t))))
      (is (empty? (seq (treaps.set/difference empty-ts empty-ts))))
      (is (empty? (seq (treaps.set/difference t t))))
      (let [t1 (apply treaps/treap-set (take 10 ks))
            t2 (apply treaps/treap-set (drop 10 ks))]
        (is (= (set (take 10 ks)) (set (seq (treaps.set/difference t1 t2)))))
        (is (= (set (drop 10 ks)) (set (seq (treaps.set/difference t2 t1))))))))

  (testing "examples from clojure.set"
    (are [x y] (= x y)
      ; identity
      (tr-difference #{}) #{}
      (tr-difference #{1}) #{1}
      (tr-difference #{1 2 3}) #{1 2 3}

      ; 2 sets
      (tr-difference #{1 2} #{1 2}) #{}
      (tr-difference #{1 2} #{3 4}) #{1 2}
      (tr-difference #{1 2} #{1}) #{2}
      (tr-difference #{1 2} #{2}) #{1}
      (tr-difference #{1 2 4} #{2 3 4 5}) #{1}

       ; 3 sets
      (tr-difference #{1 2} #{2 3} #{5 2}) #{1}
      (tr-difference #{1 2 3} #{1 3 4} #{1 3}) #{2}
      (tr-difference #{1 2 3} #{3 4 5} #{8 2 3}) #{1} )))

(deftest test-subset?
  (are [sub super] (treaps.set/subset? (apply treaps/treap-set sub) (apply treaps/treap-set super))
       #{} #{}
       #{} #{1}
       #{1} #{1}
       #{1 2} #{1 2}
       #{1 2} #{1 2 42}
       #{false} #{false}
       #{nil}   #{nil}
       #{nil}   #{nil false}
       #{1 2 nil} #{1 2 nil 4}
       )
  (are [notsub super] (not (treaps.set/subset? (apply treaps/treap-set notsub) (apply treaps/treap-set super)))
       #{1} #{}
       #{2} #{1}
       #{1 3} #{1}
       #{nil} #{false}
       #{false} #{nil}
       #{false nil} #{nil}
       #{1 2 nil}   #{1 2}))

(deftest symmetric-difference-test
  (testing "empty on one side returns the other set's elements"
    (is (= #{1 2 3}
           (treaps.set/symmetric-difference (treaps/treap-set) (treaps/treap-set 1 2 3))))
    (is (= #{1 2 3}
           (treaps.set/symmetric-difference (treaps/treap-set 1 2 3) (treaps/treap-set)))))

  (testing "both empty returns empty"
    (is (empty? (treaps.set/symmetric-difference (treaps/treap-set) (treaps/treap-set)))))

  (testing "identical instance returns empty"
    (let [t (treaps/treap-set 1 2 3)]
      (is (empty? (treaps.set/symmetric-difference t t)))))

  (testing "disjoint sets return the union of both"
    (is (= #{1 2 3 4 5 6}
           (treaps.set/symmetric-difference (treaps/treap-set 1 2 3)
                                            (treaps/treap-set 4 5 6)))))

  (testing "fully overlapping sets return empty"
    (is (empty? (treaps.set/symmetric-difference (treaps/treap-set 1 2 3)
                                                 (treaps/treap-set 1 2 3)))))

  (testing "partial overlap keeps only the non-shared elements"
    (is (= #{1 4}
           (treaps.set/symmetric-difference (treaps/treap-set 1 2 3)
                                            (treaps/treap-set 2 3 4)))))

  (testing "subset relationship leaves only the superset's extra elements"
    (is (= #{3 4}
           (treaps.set/symmetric-difference (treaps/treap-set 1 2)
                                            (treaps/treap-set 1 2 3 4)))))

  (testing "commutative regardless of argument order"
    (let [a (treaps/treap-set 1 2 3)
          b (treaps/treap-set 2 3 4)]
      (is (= (treaps.set/symmetric-difference a b)
             (treaps.set/symmetric-difference b a)))))

  (testing "single-element sets"
    (is (= #{1 2}
           (treaps.set/symmetric-difference (treaps/treap-set 1)
                                            (treaps/treap-set 2))))
    (is (empty? (treaps.set/symmetric-difference (treaps/treap-set 1)
                                                 (treaps/treap-set 1))))))
