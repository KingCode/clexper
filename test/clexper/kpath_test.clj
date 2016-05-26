(ns clexper.kpath-test
  (:require [clexper.kpath :as sut :refer [key-paths]]
            [clojure.test :as t :refer [deftest testing is]]))



(deftest basic-sequential
  (is (= [[0] [1] [2]]) (key-paths [:a :b :c])))
