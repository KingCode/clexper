# clexper

Experiments in Clojure for learning purposes, with a view to promoting modules to something usable perhaps
in their own projects.

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
