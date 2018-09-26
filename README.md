# clexper

Experiments in Clojure for learning purposes, with a view to promoting modules to something usable perhaps
in their own projects.
## N-branching tree generation: mapping a collection to its pre-order N-tuple associated elements

The latest namespace is clexper.tree.gen and has a simple/specific API: 
(gen-trees [:a :b :c]) ;; generates duples (using default 2-branching)

;;=>([[:a] [:b :c]] [[:a :b] [:c]])

(gen-trees [:a :b :c :d] ;;likewise generates

;;=> ([[:a] [[:b] [:c :d]]] [[:a] [[:b :c] [:d]]] [[:a :b] [:c :d]] [[[:a] [:b :c]] [:d]] [[[:a :b] [:c]] [:d]])

(gen-trees [:a :b :c :d] 3) ;; generates triples

;;=>([[:a] [:b] [:c :d]] [[:a] [:b :c] [:d]] [[:a :b] [:c] [:d]])

That is, all possible combinations of its leaves within N-tuples (default 2), are output so that the original elements can be read in the same order as traversing any of the output trees in pre-order fashion. 

For example, consider the noun-noun phrases that need to be considered by an NLP processing application (from http://www.naclo.cs.cmu.edu/problems2012/N2012-RS.pdf) in the following examples:
"ice cream soda":
(gen-trees [:ice :cream :soda])
;;=> ([[:ice] [:cream :soda]] [[:ice :cream] [:soda]])

"togetherness effect government study author":
(gen-trees [:togetherness :effect :government :study :author])
;;=>(.... [[:togetherness :effect] [[:government :study] [:author]]] ....)

In each case the original order remains when reading left to right. 
## Key Paths: path exploration of composite structures 

The namespace clexper.kpaths, provides navigation exploration functionality on arbitrarily nested data. 

Specifically, given any collection instance, we want to extract its navigational structure, i.e. all key sequences leading to a leaf node - an atomic value. These key sequences could then be used by 'nth, 'get-in and the likes, to retrieve leaf nodes, or prefixes thereof for sub structures instead.

For example, 
```
        (key-paths [:a :b :c])
        ;;=> [[0] [1] [2]]
        
        (key-paths [:a [:b :c] [:d [:e]]])
        ;;=> [[0] [1 0] [1 1] [2 0] [2 1 0]]
```
And on maps and sets, and mixed types (ordering may differ in result paths):

```    
       (key-paths {:a {:b {:c {:d "abcd"}}} :f "f"})
       ;;=>[[:a :b :c :d] [:f]] 
       
       (key-paths #{1 2 #{3 4}})
       ;;=> [[#{4 3} 4] [#{4 3} 3] [1] [2]]

       (key-paths '(1 {:a [2 3 {4 :four}] :b nil} #{\\A {:c :whathever}}))
       ;;=> [[0] [1 :a 0] [1 :a 1] [1 :a 2 4] [1 :b] [2 {:c :whathever} :c] [2 \A]]
```


## IndexedMap: maps with fast reverse lookup retrieval

The first module, clexper.indexing.rlookup defines the IReverseLookup protocol and extends it to regular clojure maps by wrapping a map into the IndexedMap data type.

An indexed maps is a regular map with a capability to look itself up based on functions of its values (indexers). Being regular maps, indexed maps are persistent. However all their updates (assoc/dissoc etc) yield a new indexed map with indexes consistent with the update. 

## Usage
```
;; the map to be indexed
(def phonebook  {1 {:name "Fred",        
                    :sin 11              
                    :address "f&m-a"     ;; Fred & Mary's street Address
                    :phone "111-2222"}   
                 2 {:name "Mary" 
                    :sin 22
                    :address "f&m-a"
                    :phone "111-2222"}
                 3 {:name "Pat" 
                    :sin 33
                    :address "p-a"     ;;Pat's street Address
                    :phone "333-3333"}
                 4 {:name "Alice"       ;; Fred & Mary's daughter,
                    :sin 44          
                    :address "f&m-a"    ;; still lives at home...
                    :phone "444-4444"}})  ;; but has her own phone line

;;A few indexers in a map, each ixr must have 2- and 1-arities, 2-arities is used to build 
;; an index, and the other a lookup convenience.
;; The 2-arities is required for those (admittedly rare) cases where a function may decide 
;; to e.g. skip (not index) values for some keys or take some other key-specific action.

(def indexers {:by-phone (fn phone ([_ v] #{(:phone v)}) ([v] (phone nil v)))
               :by-sin (fn sin ([_ v] #{(:sin v)}) ([v] (sin nil v)))
               :by-name (fn name ([_ v] #{(:name v)})([v] (name nil v)))
               :by-address (fn address ([_ v] #{(:address v)})([v] (address nil v)))
               :by-address&phone (fn address&phone ([_ v] #{[(:address v) (:phone v)]})
                                                ([v] (address&phone nil v)))
               :default :by-phone})


;;Create the index

(require '[clexper.indexing.rlookup :as lu])

(def phonedir (lu/make-imap phonebook indexers))

;; Lookup the keys for Mary and Alice 

 (lu/lookup-by phonedir :by-name ["Mary" "Alice"])
;;=> #{4 2}

;; or be more specific:
(lu/lookup-by phonedir :by-name ["Mary" "Alice"] :map)
;;=> {"Mary" #{2}, "Alice" #{4}}


;; everyone living at Fred and Mary's address 

(map (fn [k] (phonedir k)) (lu/lookup-by phonedir :by-address ["f&m-a"]))
;;=> ({:name "Fred", :sin 11, :address "f&m-a", :phone "111-2222"} {:name "Alice", :sin 44, :address "f&m-a", :phone "444-4444"} {:name "Mary", :sin 22, :address "f&m-a", :phone "111-2222"})

;; convenience default (in this case, by phone) look-up for phone number 111-2222:
(lu/lookup phonedir ["111-2222"]) 

;;=> #{1 2}
```
## License

Copyright © 2016 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
