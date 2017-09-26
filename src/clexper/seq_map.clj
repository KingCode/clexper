(ns clexper.seq-map)

(defprotocol InsertOrder
  (insert-order [this k]))


(defrecord InsertionOrderedMap [m order-m nextid] 
  InsertOrder
  (insert-order [this k] (get order-m k))

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

