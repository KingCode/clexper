(ns clexper.indexing.rlookup
  (:require [clojure.set :as set]))
  

(defprotocol IReverseLookup
  "A reverse lookup abstraction for using any number of indexes
   intended for use with a regular map wrapped in an implementation
   type.

   Most method signatures have an optional format argument which 
   is useful when passing multiple values for querying; it can have 
   one of three values, otherwise an implementaion specific default 
   is used:
   :aggregate all query values in a single set 
   :map query values in a map as stored in the lookup
   :seq query values in a seq in the same order as arg. lookup keys
  "
  (lookup [this rkeys fmt] [this rkeys]
    "A convenience for single index lookups: the first index
     found (or an implementation default) will be used. 
     Yields a set of all main map keys from reverse lookup keys rkeys")
  (lookup-all [this ixr-k v fmt] [this ixr-k v] [this v] 
    "Yields a set of all main map keys produced by (ixr-fn v) in lookup 
     index ixr-k using format fmt if provided; implementation  defaults 
     for ixr-k and fmt are used otherwise.") 
  (lookup-by [this ixr-k ks fmt][this ixr-k ks] 
    "Yields a set of all main map keys from lookup keys ks 
     for index keyed under ixr-k."))

(declare query update-imap ixrs&default complete-assoc complete-dissoc)

(deftype IndexedMap [m lu indexers qformat]

;; A map with (count indexers) number of reverse lookups
;; with the following fields:
;; m: a regular clojure map, on which lookups are based
;; lu: the lookups keyed by indexer keys
;; indexers: a map of indexer keys to functions, each a function
;;           with 1- and 2-arities. 2-arity is invoked on each entry
;;           at indexing/creation time, and 1-arity used for generating
;;           all reverse lookup keys for a value at query time
;; qformat: the default IReverseLookup result (query) format, 
;;          must be one of:aggregate, :map or :seq.

  IReverseLookup
  (lookup [this rkeys fmt]
    (let [[_ [ixr-k _]] (ixrs&default this)]
      (query this ixr-k rkeys fmt)))
      
  (lookup [this rkeys]
    (.lookup this rkeys qformat))
  
  (lookup-all [this ixr-k v fmt]
    (let [rkeys ((ixr-k (.indexers this)) v)]
      (query this ixr-k rkeys fmt)))

  (lookup-all [this ixr-k v]
    (.lookup-all this ixr-k v qformat))
      
  (lookup-all [this v]
    (let [[_ [ixr-k _]] (ixrs&default this)]
      (.lookup-all this ixr-k v)))

  (lookup-by [this ixr-k ks fmt] 
    (query this ixr-k ks fmt))
  
  (lookup-by [this ixr-k ks] 
    (query this ixr-k ks))

   clojure.lang.IPersistentMap
  (assoc [this k v]
    (complete-assoc this (.assoc m k v) k v))
  (assocEx [this k v] 
    (complete-assoc this (.assocEx m k v) k v))
  
  (without [this k] 
    (complete-dissoc this (.without m k) k (get m k)))

  ;; boilerplate delegation to the main map
  java.lang.Iterable
  (iterator [this]
    (.iterator m))

  clojure.lang.Associative
  (containsKey [_ k]
    (.containsKey m k))
  (entryAt [_ k]
    (.entryAt m k))

  clojure.lang.IPersistentCollection
  (count [_]
    (.count m)) 
  (cons [_ o]
    (.cons m o))
  (empty [_]
    (.empty m))
  (equiv [this o]
    (and (isa? (class o) IndexedMap)
         (.equiv o this)))

  clojure.lang.Seqable
  (seq [_]
    (.seq m))

  clojure.lang.ILookup
  (valAt [_ k]
    (.valAt m k))
  (valAt [_ k not-found]
    (.valAt m k not-found))

  clojure.lang.IFn
  (invoke [_ k] (m k))
  (invoke [_ k not-found] (m k not-found)))


(defn add-lu-entries 
" Adds lookup entries to a reverse lookup. For each entry [rk rv]
  in kvs, rv is added to the value (a main map keyset) for rk, 
  creating one if none exist.

  Yields the new reverse lookup.
"
[lu idx kvs]
  (loop [lu lu [[k v] & rest-kvs] kvs]
    (if-not k
      lu
      (let [keyset (get-in lu [idx k] #{})]
        (recur (assoc-in lu [idx k] (conj keyset v))
               rest-kvs)))))


(defn remove-lu-entries
" Removes lookup entries from a reverse lookup.
  For each entry [rk rv] in kvs, rv is removed from the value
  for rk if one exists. 
  
  Yields the new reverse lookup.
"
[lu idx kvs]
  (loop [lu lu [[k v] & rest-kvs] (seq kvs)]
    (if-not k
      lu
      (let [next-lu (if-let [keyset (get-in lu [idx k])] 
                      (if (keyset v)
                        (assoc-in lu [idx k] 
                                  (set/difference keyset #{v}))
                        lu))]
        (recur next-lu rest-kvs)))))


(defn deplete-lu-for-entry 
" Removes k (a main map key) from the keyset value from
  reverse lookup entries keyed by one of depleted-lu-keys
  in lookup lu's index keyed by idx.
  Yields the new reverse lookup.
"
[lu idx k depleted-lu-keys]
  (->> (map (fn [rk] [rk k])
            depleted-lu-keys)
       (remove-lu-entries lu idx)))


(defn depleted-keys 
[indexer-key indexer-fn k v old-v]
  (let [lu-keys (indexer-fn k v)
        old-lu-keys (indexer-fn k old-v)]
    (set/difference old-lu-keys lu-keys)))
 

(defn make-lu-entries 
[keyset k]
  (map (fn [x] [x k]) keyset))

                     
(defn add-map-entry 
" Applies the creation or changes of a map entry to its
  reverse lookup. If it is a change, old keyset values 
  are removed from the lookup accordingly.

  Yields the new reverse lookup.
"
[lu indexers k v & [old-val]]
  (loop [lu lu [& [[ixr-k ixr-fn] & rest-ixr]] indexers]

   (cond (not ixr-k) lu

         (not (fn? ixr-fn)) (recur lu rest-ixr)
         
         :else (let [lu (if old-val
                  ;; remove k from reverse lookup values' elements 
                  ;; if new look-up keys don't require it.
                          (let [depleted-lu-keys (depleted-keys ixr-k 
                                                                ixr-fn k 
                                                                v 
                                                                old-val)]
                            (deplete-lu-for-entry lu 
                                                  ixr-k 
                                                  k 
                                                  depleted-lu-keys))
                 ;; no removal needed
                          lu)]
        (recur (add-lu-entries lu 
                               ixr-k 
                               (make-lu-entries (ixr-fn k v) k))
               rest-ixr)))))


(defn remove-map-entry 
"Applies the removal of a map entry to a reverse lookup.
 For each index/er, each reverse lookup key rk in (indexer-fn k v),
 k is removed from rk's entry value (main map key set) in the index.

 Yields the new reverse lookup.
"
[lu indexers k v]
  (if-not v
    lu
    (loop [lu lu [& [[ixr-k ixr-fn] & rest-ixr]] indexers]
      (if-not ixr-k
        lu
        (let [lu-keys (ixr-fn k v)] 
          (recur (deplete-lu-for-entry lu ixr-k k (ixr-fn k v))
                 rest-ixr))))))


(defn make-imap
"Creates a map wrapping m with reverse indexes for each of indexers
 Each of indexers is a symbol+fn pair which yields a [rk rv] 
 reverse lookup entry for a (possibly derived) value of m to its key.
 
 A reverse lookup is created internally for fast retrieval using the which-keys
 function. 
"
([m indexers fmt]
 (loop [ lu {}  [& [[k v] & kvs]] (seq m)]
   (if-not k
     (->IndexedMap m lu indexers fmt)
     (recur (add-map-entry lu indexers k v)
            kvs))))
([m indexers]
 (make-imap m indexers :aggregate)))


(defn- complete-assoc [imap new-m k v]
  (update-imap imap new-m k v :put))


(defn- complete-dissoc [imap new-m k v]
  (update-imap imap new-m k v :remove))


(defn update-imap
"Yields a new IndexedMap from imap using m for its main map
 and a new reverse lookup resulting from applying the change.
 If v is provided a new entry [k v] is added, otherwise 
 the entry for k is removed.
"
[imap m k v op]
(let [op (case op
           :put add-map-entry
           :remove remove-map-entry)]
      (->IndexedMap m
                    (op (.lu imap) 
                        (seq (.indexers imap)) 
                        k v)
                    (.indexers imap)
                    (.qformat imap))))


(defn ixrs&default [imap]
  (->> (.indexers imap)
       ((fn [ixrs] [ixrs, (if-let [dixr (:default ixrs)]
                            (if (fn? dixr)
                              [:default dixr]
                              [dixr (dixr ixrs)])
                            (first ixrs))]))))


(defn query 
  ([imap ixr-k rkeys fmt]
   (let [sub-lu (select-keys (ixr-k (.lu imap)) rkeys)]
     (case fmt
       :aggregate (apply set/union (vals sub-lu))
       :seq (map #(get sub-lu %) rkeys)
       :map sub-lu
       (ex-info "query format not recognized or provided" 
                {:ixr-k ixr-k
                 :rkeys rkeys
                 :format fmt}))))
  ([imap ixr-k rkeys]
   (query imap ixr-k rkeys (.qformat imap))))
