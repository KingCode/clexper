(ns clexper.kpath-test
  (:require [clexper.kpath :as sut :refer [key-paths]]
            [clojure.test :as t :refer [deftest testing is are]]))



(deftest basic-sequential
  (are [x y] (= x (key-paths y))
    [[0] [1] [2]] [:a :b :c]
    [[0] [1 0]] [:a [:b]]
    [[0] [1 0] [1 1]] [:a [:b :c]]
    [[0] [1 0] [1 1 0] [1 1 1 0]] [:a [:b [:c [:d]]]]
    [[0] [1 0] [1 1 0] [1 1 1 0] [2] [3 0] [3 1]] [:a [:b [:c [:d]]] :e [:f :g]]))

