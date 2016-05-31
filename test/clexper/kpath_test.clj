(ns clexper.kpath-test
  (:require [clexper.kpath :as sut :refer [paths]]
            [clojure.test :as t :refer [deftest testing is are]]))



(deftest sequential-test
  (are [x y] (= x (paths y))
    [[0] [1] [2]] [:a :b :c]
    [[0] [1 0]] [:a [:b]]
    [[0] [1 0] [1 1]] [:a [:b :c]]
    [[0] [1 0] [1 1 0] [1 1 1 0]] [:a [:b [:c [:d]]]]
    [[0] [1 0] [1 1 0] [1 1 1 0] [2] [3 0] [3 1]] [:a [:b [:c [:d]]] :e [:f :g]]

    [[0] [1] [2]] '(:a :b :c)
    [[0] [1 0] [1 1 0] [1 1 1 0]] '(:a (:b (:c (:d))))))


(deftest map-test
  (are [x y] (= (set x) (set (paths y)))
    [[:a] [:b] [:c]] {:a "a" :b "b" :c "c"}  
    [[:a :b]] {:a {:b "ab"}}
    [[:a :b :c :d]] {:a {:b {:c {:d "abcd"}}}}
    [[:a :b :c :d] [:f]] {:a {:b {:c {:d "abcd"}}} :f "f"}))
