(ns clexper.indexing.rlookup-test
  (:require [clexper.indexing.rlookup 
             :refer [lookup lookup-all lookup-by
                     make-imap] :as sut]
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
            (is (= lu-3-expected lu-3-actual))
            (is (= lu-2-expected lu-2-actual))
            (is (= lu-1-expected lu-1-actual)))
        :default {:a 1, :b 2} :map {:a #{:foo :bar} :b #{:foo :baz}},
        :default {:a 30, :bob "whatever" :etc nil :c nil} #{:foo :bar :baz}, 
        {:b 3 :c 5} #{:foo :bar :baz}))
    
    (testing "arities of query 'lookup-by on IndexedMap"
      (are [lu-3-ixk lu-3-keys lu-3-fmt lu-3-expected,
            lu-2-ixk lu-2-keys lu-2-expected]
          (let [lu-3-actual (lookup-by imap lu-3-ixk lu-3-keys lu-3-fmt)
                lu-2-actual (lookup-by imap lu-2-ixk lu-2-keys)]
            (is (= lu-3-expected lu-3-actual))
            (is (= lu-2-expected lu-2-actual)))
       :default [:a :c] :seq '(#{:foo :bar} #{:bar :baz}),
       :default [:b :a] #{:foo :bar :baz}))))
    

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
         :default :phone}))

(def phonebook  {1 {:name "Fred",        ;; *** Mnemonics: ***
                    :sin 11              ;; reflects key
                    :address "f&m-a"     ;; Fred & Mary's street Address
                    :phone "111-2222"}   ;; made of of sins digits
                 2 {:name "Mary" 
                    :sin 22
                    :address "f&m-a"
                    :phone "111-2222"}
                 3 {:name "Pat" 
                    :sin 33
                    :address "psa"
                    :phone "333-3333"}
                 4 {:name "Bobby"
                    :sin 44
                    :address "bsa"
                    :phone "444-4444"}
                 5 {:name "Alice"    ;; Alice is the daughther of 
                    :sin 55          ;;  Fred & Mary,
                    :address "f&m-a" ;; still lives at home...
                    :phone "555-5555"}   ;; but has her own phone line
                 })

(defmacro run-test 
  ([f imap ixrk ks fmt expected msg]
   `(is (= ~expected (~f ~imap ~ixrk ~ks ~fmt)) ~msg))
  ([f imap ixrk ks expected msg]
   `(is (= ~expected (~f ~imap ~ixrk ~ks)) ~msg))
  ([f imap v-or-k expected msg]
   `(is (= ~expected (~f ~imap ~v-or-k)) ~msg)))

(deftest multiple-index-test
  (let [pbdir (make-imap phonebook phone-ixrs)]
    (is (= phonebook (.m pbdir)))
    (testing "'lookup-by for single-keyed reverse lookups"
      (are [lu-3-ixrk lu-3-ks lu-3-fmt lu-3-expected,
            lu-2-ixrk lu-2-ks lu-2-expected]
          (let [lu-3-actual (lookup-by pbdir lu-3-ixrk lu-3-ks lu-3-fmt)
                lu-2-actual (lookup-by pbdir lu-2-ixrk lu-2-ks)]
            (is (= lu-3-expected lu-3-actual))
            (is (= lu-2-expected lu-2-actual)))
          #_(do 
            (run-test lookup-by pbdir lu-3-ixrk lu-3-ks lu-3-fmt lu-3-expected "lu-3-actual")
            (run-test lookup-by pbdir lu-2-ixrk lu-2-ks lu-2-expected "lu-2-actual"))
          :by-name ["Fred"] :map {"Fred" #{1}}
          :by-name ["Fred"] #{1},
          
          :by-address ["f&m-a" "bsa"] :seq '(#{1 2 5} #{4})
          :by-address ["f&m-a" "bsa"] #{1 2 4 5}))

    (testing "'lookup-by for multi-keyed reverse lookups"
      (are [lu-3-ixrk lu-3-ks lu-3-fmt lu-3-expected]
          (let [lu-3-actual (lookup-by pbdir lu-3-ixrk lu-3-ks lu-3-fmt)]
            (is (= lu-3-expected lu-3-actual)))
        :by-name&address [["Fred" "f&m-a"] ["Pat" "psa"]] :map {["Fred" "f&m-a"] #{1}
                                                                ["Pat" "psa"] #{3}},
        :by-address&phone [["f&m-a" "111-2222"]] :aggregate #{1 2},
        :by-name&address&phone [["Bobby" "bsa" "444-4444"] ["Pat" "psa" "333-3333"]] :seq
         '(#{4} #{3})))))
