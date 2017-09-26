(ns clexper.tree.gen
  (:require [clojure.math.combinatorics :as combo]))

;; Marker type for source values
(defrecord Leaf [value])

(defmethod print-method Leaf [l w]
  (print-method (:value l) w)
  #_(print-method "^" w))

(defn leafy? [coll] (instance? Leaf (first coll)))

(defn branch? [coll n]
  (let [c (count coll)] 
    (and (pos? c)
         (<= c n))))

(defn subtree? 
"Returns true if coll is a valid n-branching tree"
  ([coll n]
   (cond 
     (leafy? coll) 
     (branch? coll n) 
     (branch? coll n)
     (every? #(subtree? % n) coll)
     :else false))
  ([coll]
   (subtree? coll 2)))


(defn as-leafy 
"Marks elements of coll as leaves"
[coll]
  (->> (vec coll)
       (mapv (fn [x]
               (->Leaf x)))))

(declare splitv-ats)

(defn splitv-at 
"i can be either a positive int or an ordered, no-dupes seq thereof.
 Splits coll at index i into two subvectors; if i is a seq,
 splits coll at respective indexes"
[i coll] 
  (if (sequential? i)
    (splitv-ats i coll)
    (let [v (vec coll)]
      [(subvec v 0 i),
       (subvec v i)])))

(defn diffs 
"'is is a vector of ordered positive ints.
 Yields a vector of length (count is) of the (positive) 
 difference b/w each pair of neigbours. The first element
 is unmodified, e.g. (diffs [1 3 4 5] -> [1 2 1 1]"
[is]
  (->> is 
       (reduce (fn [[xis prev] i]
                 [(conj xis (- i prev)), i])
               [[] 0])
       first))

(defn splitv-ats 
"Implements splitv-at for multiple splitting indexes."
[is v]
  (let [rest-is (diffs is)]
    (->> rest-is
         (reduce (fn [[splits tail] i]
                   (let [new-split (splitv-at i tail)]
                     [(conj splits (first new-split)),
                      (last new-split)]))
                 [[],v])
         (apply conj))))

(defn splits 
"Generates all possible n-way splits (default 2) of coll as vectors"
  ([coll n]
   (if (<= (count coll) n)
     [coll]
     (->> (combo/combinations (range 1 (count coll))
                              (dec n))
          (mapv (fn [i]
                  (splitv-at i coll))))))
  ([coll]
   (splits coll 2)))

(declare map-splits)

(defn map-split 
"'split is an element of the result of a splitting operation.
  Yields split if it is a conforming n-branching subtree (default 2), 
  or a map of each of its parts to themselves or maps of their
 parts thereof etc..
 "
  ([split n]
   (if (subtree? split n) 
     split
     (->> split
          (reduce (fn [m e]
                    (assoc m e (map-splits e n)))
                  {}))))
  ([split]
   (map-split split 2)))

(defn map-splits 
"'coll is a vector of leaves.
 Yields coll if it is a conforming n-branching tree;
 otherwise all n-splits partitions of coll are generated
 and a map of each partition (a 'split' from all splits)
 to a map of its splits is returned - see map-split."
  ([coll n]
   (if (subtree? coll n)
     coll
     (->> (splits coll n)
          (map (fn [split]
                 [split (map-split split n)]))
          (into {}))))
  ([coll]
   (map-splits coll 2)))

(defn restore-leaves 
"Unwraps each leaf inside coll to restore the original value."
[coll]
  (if (leafy? coll)
    (mapv :value coll)
    (mapv restore-leaves coll)))


;;;;;;;;;;;;;;; Tree Generation ;;;;;;;;;;;;;;;;

(declare map->trees parse->trees)

(defn entry->trees 
"Generates trees from a map entry.
 The source map is the result of invoking
 map-splits on a leafy source coll."
[[k v]]
 (cond 
   (= k v) ;;terminal tree
   [(restore-leaves v)] 
   (leafy? k) ;; raw/top-level structure: expand it
   (map->trees v)
   :else ;; hybrid of the above: parse key's elements
   (parse->trees k v)))

(defn parse->trees 
"Generates trees from an ordered sequence of parts
 (a n-part split of some super coll, containing either 
 leafy elements or n-part splits), and their mapping 
 to their respective subtrees."
[coll m]
  (->> coll
       (map #(entry->trees [%, (get m %)]))
       (apply combo/cartesian-product)
       (map vec)))

(defn map->trees 
"Generates trees from a split mapping's value map"
[m]
  (mapcat entry->trees m))

(defn singleton 
"Singleton mapping of a top-level leafy coll to its splits map."
[k v]
  {k v})

;;;;;;;;;;;;;;; Public API ;;;;;;;;;;;;;;;;;;;;;;;;

(defn gen-trees 
"Generates all trees with branching factor n (defaults to 2) 
 with coll elements as leaves having in-order traversal 
 the same as ordering in coll"
  ([coll n]
   (cond 
     (< 1 n)
     (let [leafy (as-leafy coll)]
       (-> leafy
           (map-splits n)
           (#(singleton leafy %))
           map->trees))
     (= 1 n)
     (list coll)
     
     :else
     (throw (ex-info "Can't split into less than one branch" {:n-arg n}))))
  ([coll]
   (gen-trees coll 2)))

;; REPL testing 
#_(comment "REPL test fixtures" 
  (def ab (as-leafy [:a :b]))
  (def ab-m (map-splits ab))
  (def ab-t (gen-trees (restore-leaves ab)))
  (def abc (as-leafy [:a :b :c]))
  (def abc-m (map-splits abc))
  (def abc-t (gen-trees (restore-leaves abc)))
  (def abcd (as-leafy [:a :b :c :d]))
  (def abcd-m (map-splits abcd))
  (def abcd-t (gen-trees (restore-leaves abcd)))
  (def abcde (as-leafy [:a :b :c :d :e]))
  (def abcde-m (map-splits abcde))
  (def abcde-t (gen-trees (restore-leaves abcde)))
  (def abcdef (as-leafy [:a :b :c :d :e :f]))
  (def abcdef-m (map-splits abcdef))
  (def abcdef-t (gen-trees (restore-leaves abcdef)))
  (def abcdef-3m (map-splits abcdef 3))
  (def abcdef-3t (gen-trees (restore-leaves abcdef) 3)))
(defn leaf-groupings [coll n]
  (println coll)
  (if (and (leafy? coll) 
           (<= (count coll) n))
    coll
    (let [splits (splits coll n)]
      (mapv #(leaf-groupings % n) splits))))

(defn deleaf [coll]
  (if (leafy? coll)
    (mapv :value coll)
    (mapv deleaf coll)))

(defn groupings [coll n]
  (let [groups (leaf-groupings (as-leafy coll) n)]
    (deleaf groups)))
