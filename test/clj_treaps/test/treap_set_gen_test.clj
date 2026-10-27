(ns clj-treaps.test.treap-set-gen-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.set :as cset]
            [fogus.treaps :as treaps]
            [fogus.treaps.set :as tset]))

;; Run with test.check on the test classpath. These tests intentionally use
;; integer keys so failures are about set semantics, not comparator behavior.

(def gen-int-set
  (gen/fmap set (gen/vector (gen/choose -100 100) 0 40)))

(def gen-set-seq
  (gen/vector gen-int-set 0 6))

(defn as-treap
  "Convert a regular Clojure set to the TreapSet type expected by tset ops."
  [s]
  (treaps/->treap-set s))

(defn as-treaps
  [sets]
  (mapv as-treap sets))

(defn contents
  "Normalize either implementation to an ordinary Clojure set."
  [s]
  (set (seq s)))

(defn same-set?
  [expected actual]
  (and (instance? fogus.treaps.TreapSet actual)
       (= expected (contents actual))
       (= (count expected) (count actual))))

(defspec treap-construction-matches-clojure-set 500
  (prop/for-all [xs (gen/vector (gen/choose -100 100) 0 80)]
    (let [expected (set xs)
          actual   (apply treaps/treap-set xs)
          poured   (treaps/->treap-set xs)]
      (and (same-set? expected actual)
           (same-set? expected poured)
           (= expected actual)
           (= actual expected)
           (= (hash expected) (hash actual))
           (= (hash actual) (hash expected))))))

(defspec membership-lookup-count-and-seq-match 500
  (prop/for-all [s gen-int-set
                 probes (gen/vector (gen/choose -120 120) 0 60)
                 not-found (gen/one-of [(gen/return ::missing)
                                        (gen/choose -1000 1000)])]
    (let [ts (as-treap s)]
      (and (= (count s) (count ts))
           (= s (contents ts))
           (every? (fn [x]
                     (and (= (contains? s x) (contains? ts x))
                          (= (get s x) (get ts x))
                          (= (get s x not-found) (get ts x not-found))
                          (= (s x) (ts x))))
                   probes)
           (= (empty? s) (empty? ts))))))

(defspec conj-disj-and-persistence-match 500
  (prop/for-all [s gen-int-set
                 additions (gen/vector (gen/choose -120 120) 0 50)
                 removals (gen/vector (gen/choose -120 120) 0 50)]
    (let [ts (as-treap s)
          expected-added (into s additions)
          actual-added (into ts additions)
          expected-removed (reduce disj expected-added removals)
          actual-removed (reduce disj actual-added removals)]
      (and (same-set? expected-added actual-added)
           (same-set? expected-removed actual-removed)
           (= s (contents ts))
           (= expected-added (contents actual-added))
           (= expected-removed (contents actual-removed))))))

(defspec equality-and-hashing-match 500
  (prop/for-all [a gen-int-set
                 b gen-int-set]
    (let [ta (as-treap a)
          tb (as-treap b)]
      (and (= (= a b) (= ta tb))
           (= (= a b) (= ta b))
           (= (= a b) (= a tb))
           (= (hash a) (hash ta))
           (= (hash b) (hash tb))
           (or (not= a b) (= (hash ta) (hash tb)))))))

(defspec empty-and-metadata-behavior 250
  (prop/for-all [s gen-int-set
                 marker (gen/choose -1000 1000)]
    (let [ts (as-treap s)
          tagged (with-meta ts {:marker marker})
          emptied (empty tagged)]
      (and (instance? fogus.treaps.TreapSet (empty ts))
           (empty? (empty ts))
           (= (meta tagged) (meta emptied))
           (= s (contents tagged))
           (= s (contents (with-meta tagged {:other true})))
           (= (meta (with-meta tagged {:other true})) {:other true})))))

(defspec union-matches-clojure-set 500
  (prop/for-all [sets gen-set-seq]
    (let [expected (apply cset/union sets)
          treaps* (as-treaps sets)
          actual (apply tset/union treaps*)]
      (and (same-set? expected actual)
           (same-set? (apply cset/union sets) (apply tset/union treaps*))))))

(defspec intersection-matches-clojure-set 500
  (prop/for-all [sets (gen/vector gen-int-set 1 6)]
    (let [expected (apply cset/intersection sets)
          actual (apply tset/intersection (as-treaps sets))]
      (same-set? expected actual))))

(defspec difference-matches-clojure-set 500
  (prop/for-all [sets (gen/vector gen-int-set 1 6)]
    (let [expected (apply cset/difference sets)
          actual (apply tset/difference (as-treaps sets))]
      (same-set? expected actual))))

(defspec symmetric-difference-matches-xor-reference 500
  (prop/for-all [a gen-int-set
                 b gen-int-set]
    (let [expected (cset/union (cset/difference a b)
                               (cset/difference b a))
          actual (tset/symmetric-difference (as-treap a) (as-treap b))]
      (same-set? expected actual))))

(defspec subset-and-superset-match-clojure-set 500
  (prop/for-all [a gen-int-set
                 b gen-int-set]
    (let [ta (as-treap a)
          tb (as-treap b)]
      (and (= (cset/subset? a b) (tset/subset? ta tb))
           (= (cset/superset? a b) (tset/superset? ta tb))
           (= (cset/subset? a b) (cset/superset? b a))
           (= (cset/superset? a b) (cset/subset? b a))))))

(deftest algebra-arity-and-edge-cases
  (testing "union supports zero and one argument"
    (is (same-set? #{} (tset/union)))
    (is (same-set? #{} (tset/union (as-treap #{}))))
    (is (same-set? #{1 2} (tset/union (as-treap #{1 2})))))

  (testing "single-argument intersection and difference preserve the set"
    (is (same-set? #{1 2} (tset/intersection (as-treap #{1 2}))))
    (is (same-set? #{1 2} (tset/difference (as-treap #{1 2})))))

  (testing "empty, identical, disjoint, and overlapping operands"
    (let [empty-ts (as-treap #{})
          a (as-treap #{1 2 3})
          same-a (as-treap #{1 2 3})
          b (as-treap #{3 4})]
      (is (same-set? #{} (tset/intersection empty-ts a)))
      (is (same-set? #{1 2 3} (tset/union empty-ts a)))
      (is (same-set? #{} (tset/difference a same-a)))
      (is (same-set? #{1 2 4} (tset/symmetric-difference a b)))
      (is (tset/subset? empty-ts a))
      (is (tset/superset? a empty-ts))
      (is (not (tset/subset? a b)))
      (is (not (tset/superset? a b))))))

(deftest java-set-interface-is-read-only
  (let [ts (as-treap #{1 2 3})]
    (is (.contains ^java.util.Set ts 2))
    (is (.containsAll ^java.util.Set ts [1 3]))
    (is (= 3 (.size ^java.util.Set ts)))
    (is (thrown? UnsupportedOperationException (.add ^java.util.Set ts 4)))
    (is (thrown? UnsupportedOperationException (.remove ^java.util.Set ts 2)))
    (is (thrown? UnsupportedOperationException (.clear ^java.util.Set ts)))))
