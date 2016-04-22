(ns clexper.indexing.rlookup
  (:require [clojure.set :as set])
  (:import clojure.lang.IPersistentMap))
  

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
     index ixr-k. If using the 2-arity the first found index and ix-fn
     will be used.") 
  (lookup-by [this ixr-k ks fmt][this ixr-k ks] 
    "Yields a set of all main map keys from lookup keys ks 
     for index keyed under ixr-k."))

(declare query update-imap ixrs&default)

(deftype IndexedMap [main lookup indexers qformat]

;; A map with (count indexers) number of reverse lookups
;; with the following fields:
;; main: a regular clojure map, on which lookups are based
;; lookup: the lookups keyed by indexer keys
;; indexers: a map of indexer keys to functions, each a function
;;           with 1- and 2-arities. 2-arity is invoked on each entry
;;           at indexing/creation time, and 1-arity used for generating
;;           all reverse lookup keys for a value at query time
;; qformat: the default IReverseLookup format, must be one of
;;         :aggregate, :map or :seq.

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
              (lookup-all this v ixr-k qformat))
      
  (lookup-all [this v]
              (let [[_ [ixr-k ixr]] (ixrs&default this)
                    rkeys (ixr v)]
                (query this ixr-k rkeys)))

  (lookup-by [this ixr-k ks fmt] 
             (query this ixr-k ks fmt))

  (lookup-by [this ixr-k ks] 
             (query this ixr-k ks))

   IPersistentMap
  (assoc [this k v]
    (update-imap this (.assoc main k v) k v))
  (assocEx [this k v] 
    (update-imap this (.assocEx main k v) k v))
  
  (without [this k] 
    (update-imap this (.without main k) k))

  ;; boilerplate delegation to main-map
  java.lang.Iterable
  (iterator [this]
    (.iterator main))

  clojure.lang.Associative
  (containsKey [_ k]
    (.containsKey main k))
  (entryAt [_ k]
    (.entryAt main k))

  clojure.lang.IPersistentCollection
  (count [_]
    (.count main)) 
  (cons [_ o]
    (.cons main o))
  (empty [_]
    (.empty main))
  (equiv [this o]
    (and (isa? (class o) IndexedMap)
         (.equiv o this)))

  clojure.lang.Seqable
  (seq [_]
    (.seq main))

  clojure.lang.ILookup
  (valAt [_ k]
    (.valAt main k))
  (valAt [_ k not-found]
    (.valAt main k not-found))

  clojure.lang.IFn
  (invoke [_ k] (main k))
  (invoke [_ k not-found] (main k not-found)))


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
      (let [next-lu (if-let [keyset (get-in lu idx k)] 
                      (if (keyset v)
                        (assoc-in lu [idx k] 
                                  (set/difference keyset #{v}))
                        lu))]
        (recur next-lu rest-kvs)))))


(defn deplete-lu-for-entry 
" Removes k (a main map key) from the keyset value 
  from reverse lookup entries keyed by one of depleted-lu-keys.

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

   (if-not ixr-k
     lu
     (let [lu (if old-val
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


(defn update-imap
"Yields a new IndexedMap from imap using m for its main map
 and a new reverse lookup resulting from applying the change.
 If v is provided a new entry [k v] is added, otherwise 
 the entry for k is removed.
"
[imap m k & [v]]
  (let [op (if v 
             add-map-entry 
             remove-map-entry)]
    (->IndexedMap m
                  (op (.lookup imap) 
                      (seq (.indexers imap)) 
                      k v)
                  (.indexers imap)
                  (.qformat imap))))

(defn ixrs&default [imap]
  (->> (.indexers imap)
       ((fn [ixrs] [ixrs (first ixrs)]))))

(defn query-as-seq [imap ixr-k rkeys]
  (map #(get-in (.lookup imap)
                     [ixr-k %]) 
            rkeys))


(defn query 
  ([imap ixr-k rkeys fmt]
   (let [op (case fmt
              :aggregate #(reduce into #{} %)
              :seq identity
              :map #(zipmap rkeys %)
              (ex-info "query format not recognized or provided" 
                       {:ixr-k ixr-k
                        :rkeys rkeys
                        :format fmt}))] 
     (op (query-as-seq imap ixr-k rkeys))))
  ([imap ixr-k rkeys]
   (query imap ixr-k rkeys (.qformat imap))))
