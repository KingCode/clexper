(ns clexper.indexing.rlookup-test
  #_(:import [clexper.indexing.rlookup IndexedMap])
  (:require [clexper.indexing.rlookup :refer [make-imap] :as sut]
            [clojure.test :refer :all]))



(deftest single-index-test
  (let [m {:foo {:a 1, :b 2}, :bar {:a 2, :c 4}, :baz {:b 3, :c 5}}
        ixr (fn [k v] (->> (set (keys v))))
        imap (make-imap m {:default  ixr})]
    (is (= m (.main imap)))
    (is (= {:default {:a #{:foo :bar}
                      :b #{:foo :baz}
                      :c #{:bar :baz}}} 
           (.lookup imap)))

    (testing  "arities of protocol method lookup on IndexedMap"
      (are [lu-2-keys lu-2-fmt lu-2-expected, lu-1-keys lu-1-expected]
          (let [lu-2-actual (.lookup imap lu-2-keys lu-2-fmt)
                lu-1-actual (.lookup imap lu-1-keys)]
            (is (= lu-2-expected lu-2-actual))
            (is (= lu-1-expected lu-1-actual)))
       [:a] :map {:a #{:foo :bar}}, [:a] #{:foo :bar}
       [:a] :seq '(#{:foo :bar}),   [:b] #{:foo :baz}
       [:a :b] :map {:a #{:foo :bar} :b #{:foo :baz}}, [:a :b] #{:foo :bar :baz}
))))
      
         
(def phonebook  {1 {:name "Fred", 
                    :sin 11
                    :address "fred's street"
                    :phone "fred's phone"} 
                 2 {:name "Mary" 
                    :sin 22
                    :address "mary's street"
                    :phone "mary's phone"} 
                 3 {:name "Pat" 
                    :sin 33
                    :address "pat's street"
                    :phone "pat's phone"}
                 4 {:name "Bobby"
                    :sin 44
                    :address "bobby's street"
                    :phone "bobby's phone"}})

(def phone-ixrs {:default (fn [k v] 
                            #{(:phone v)}) 
                 :by-name (fn [k v] 
                            #{(:name v)})})

#_(deftest multiple-index-test


)
    

