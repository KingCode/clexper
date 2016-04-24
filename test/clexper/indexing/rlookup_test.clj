(ns clexper.indexing.rlookup-test
  (:import (clexper.indexing.rlookup IndexedMap))
  (:require [clexper.indexing.rlookup :refer [IReverseLookup make-imap] :as sut]
            [clojure.test :refer :all]))

(deftest single-index-test
  (let [m {:foo {:a 1, :b 2}, :bar {:a 2, :c 4}, :baz {:b 3, :c 5}}
        ixr (fn f 
              ([_ v] (->> (set (keys v))))
              ([v] (f nil v)))
        imap (make-imap m {:default  ixr})]
    (testing "imap creation and fields"
      (is (= m (.m imap)))
      (is (= {:default {:a #{:foo :bar}
                        :b #{:foo :baz}
                        :c #{:bar :baz}}} 
             (.lu imap))))
    
    (testing  "arities of query method 'lookup on IndexedMap"
      (are [lu-2-keys lu-2-fmt lu-2-expected, lu-1-keys lu-1-expected]
          (let [lu-2-actual (lookup imap lu-2-keys lu-2-fmt)
                lu-1-actual (lookup imap lu-1-keys)]
            (is (= lu-2-expected lu-2-actual))
            (is (= lu-1-expected lu-1-actual)))
       [:a] :map {:a #{:foo :bar}}, [:a] #{:foo :bar}
       [:a] :seq '(#{:foo :bar}),   [:b] #{:foo :baz}
       [:a :b] :map {:a #{:foo :bar} :b #{:foo :baz}}, [:a :b] #{:foo :bar :baz}
       [:b :c] :seq '(#{:foo :baz} #{:bar :baz}), [:a :c] #{:foo :bar :baz}))
    

    (testing  "arities of query 'lookup-all on IndexedMap"
      (are [lu-3-ixk lu-3-v lu-3-fmt lu-3-expected, 
            lu-2-ixk lu-2-v lu-2-expected,
            lu-1-v lu-1-expected]
          (let [lu-3-actual (lookup-all imap lu-3-ixk lu-3-v lu-3-fmt)
                lu-2-actual (lookup-all imap lu-2-ixk lu-2-v)
                lu-1-actual (lookup-all imap lu-1-v)]
            #_(is (= lu-3-expected lu-3-actual))
            (is (= lu-2-expected lu-2-actual))
            (is (= lu-1-expected lu-1-actual)))
        :default {:a 1, :b 2} :map {:a #{:foo :bar} :b #{:foo :baz}},
        :default {:a 30, :bob "whatever" :etc nil :c nil} #{:foo :bar :baz}, 
        {:b 3 :c 5} #{:foo :bar :baz}))))
    

    ;; phonebook indexing fns
    (def phone-ixrs 
      (letfn [(phone
                ([_ v] #{(:phone v)})
                ([v] (phone nil v))) 
              (name
                ([_ v] #{(:name v)})
                ([v] (name nil v)))
              (address 
                ([_ v] #{(:address v)})
                ([v] (address nil v)))
              (sin
                ([_ v] #{(:sin v)})
                ([v] (sin nil v)))
              (name&address 
                ([_ v] #{[(:name v) (:address v)]})
                ([v] (name&address nil v)))
              (address&phone
                ([_ v] #{[(:address v) (:phone v)]})
                ([v] (address&phone nil v)))
              (name&phone
                ([_ v] #{[(:name v) (:phone v)]})
                ([v] (name&phone nil v)))
              (name&address&phone
                ([_ v] #{[(:name v) (:address v) (:phone v)]})
                ([v] (name&address&phone nil v)))]
        {:by-name name
         :by-address address
         :by-phone phone
         :by-sin sin
         :by-name&address name&address
         :by-address&phone address&phone
         :by-name&phone name&phone
         :by-name&address&phone name&address&phone
         :default phone}))

(def phonebook  {1 {:name "Fred", 
                    :sin 11              ;; mnemonics:
                    :address "f&m-a"     ;; Fred and Mary Street Address
                    :phone "f&m-p1122"}  ;; Fred and Mary 
                 2 {:name "Mary" 
                    :sin 22
                    :address "f&m-a"
                    :phone "f&m-p1122"}
                 3 {:name "Pat" 
                    :sin 33
                    :address "psa"
                    :phone "pp33"}
                 4 {:name "Bobby"
                    :sin 44
                    :address "bsa"
                    :phone "bp44"}
                 5 {:name "Alice"    ;; Alice is the daughther of 
                    :sin 55          ;;  Fred & Mary,
                    :address "f&m-a" ;; still lives at home...
                    :phone "ap55"}   ;; but has her own phone line
                 })

(deftest multiple-index-test
  (let [imap (make-imap phonebook phone-ixrs)])

)
    

