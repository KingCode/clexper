(ns clexper.kpath-test
  (:require [clexper.kpath :as sut :refer [paths]]
            [clojure.test :as t :refer [deftest testing is are]]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))


(deftest paths-sequential-test
  (are [x y] (= x (paths y))
    [[0] [1] [2]] [:a :b :c]
    [[0] [1 0]] [:a [:b]]
    [[0] [1 0] [1 1]] [:a [:b :c]]
    [[0] [1 0] [1 1 0] [1 1 1 0]] [:a [:b [:c [:d]]]]
    [[0] [1 0] [1 1 0] [1 1 1 0] [2] [3 0] [3 1]] [:a [:b [:c [:d]]] :e [:f :g]]

    [[0] [1] [2]] '(:a :b :c)
    [[0] [1 0] [1 1 0] [1 1 1 0]] '(:a (:b (:c (:d))))))


(deftest paths-map-test
  (are [x y] (= (set x) (set (paths y)))
    [[:a] [:b] [:c]] {:a "a" :b "b" :c "c"}  
    [[:a :b]] {:a {:b "ab"}}
    [[:a :b :c :d]] {:a {:b {:c {:d "abcd"}}}}
    [[:a :b :c :d] [:f]] {:a {:b {:c {:d "abcd"}}} :f "f"}))

(deftest paths-sets-test
  (are [x y] (= (set x) (set (paths y)))
    [[1] [2] [3]] #{1 2 3}
    [[:a] [#{:b} :b]] #{:a #{:b}}
    [[:a] [#{:b #{:c}} :b] [#{:b #{:c}} #{:c} :c]] #{:a #{:b #{:c}}}))


(deftest paths-mixed-test
  (are [x y] (= (set x) (set (paths y)))
    [[0] [1 0] [1 1 2 3] [1 1 2 4] [1 2]] [0 [1 {2 #{3 4}} 5]]
;; expected
    [[:a 0] 
     [:a 1 1] 
     [:a 1 {:c [:d :e]} :c 0]
     [:a 1 {:c [:d :e]} :c 1] 
     [:a 2 0]
     [:a 2 1]] 
;; actual    
    {:a [:b #{1 {:c [:d :e]}} [:f :g]]}))


#_(defspec first-element-is-min-after-sorting ;; the name of the test
         100 ;; the number of iterations for test.check to test
         (prop/for-all [v (gen/not-empty (gen/vector gen/int))]
           (= (apply min v)
              (first (sort v)))))
