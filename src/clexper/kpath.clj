(ns clexper.kpath)


(defn show-unwrap [all paths p] 
  (println (str "UNWRAP (ALL=" all ", PATHS=" paths ", P=" p)))

(defn show-subpaths [k v prefix kps] 
  (println (str "SUBPATHs (" "K=" k ", V=" v ", PFX=" prefix "\n\tKPATHS for v=" kps)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extracting all paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare paths)

(defn subpaths [k v prefix]               
  (let [subp (paths v)]
    (if (seq subp)
      (->> subp
           (mapv #(apply conj prefix k %)))
      (conj prefix [k]))))

(defn unpack [packed-paths]
  (reduce #(into % %2) [] packed-paths))

(defn paths-with-indexes 
  ([c prefix]
   (->> c
        (mapv (fn [i e]
                (subpaths i e prefix))
              (range))
        unpack))
  ([c] (paths-with-indexes c [])))

(defn paths-with-keys 
  ([c prefix]
   (->> c
        (mapv (fn [[k v]] 
                (subpaths k v prefix)))
        unpack))
  ([c] (paths-with-keys c [])))

(defn paths-with-values 
  ([c prefix]
   (->> c 
        (mapv (fn [v]
                (subpaths v v prefix)))
        unpack))
  ([c] (paths-with-values c [])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Retrieving values from key paths           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare get-at)
(defn get-with-indexes [c [i & is]]
  (let [v (nth c i)]
    (if-not (seq is)
      v
      (get-at v is))))

(defn get-with-keys [c [k & ks]]
  (let [v (get c k)]
    (if-not (seq ks)
      v
      (get-at v ks))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Finding a path from a value 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare somep)

(defn some-subpath [k v prefix search]               
  (if-let [subpath (or (when (= search v) ()) 
                       (somep v search))]
        (apply conj prefix k subpath)
    nil))

(defn some-path-impl [c prefix indexer-fn search]
  (let [find (comp indexer-fn 
                   (keep (fn [[idx v]]
                             (some-subpath idx v prefix search))))]
    (first (transduce find conj c))))
        
(defn some-path-using-indexes 
  ([c prefix search]
   (let [counter (atom -1)] 
     (some-path-impl c, prefix, 
                     (map (fn [v]
                            [(swap! counter inc), v])), 
                     search)))
  ([c search]
   (some-path-using-indexes c [] search)))

(defn some-path-using-keys 
  ([c prefix search]
   (some-path-impl c prefix identity search))
  ([c search]
   (some-path-using-keys c [] search)))

(defn some-path-using-values
  ([c prefix search]
   (some-path-impl c, prefix,
                   (map (fn [v] [v,v])),
                   search))
  ([c search]
   (some-path-using-values c [] search)))


(defn some-path-using-nothing
  ([c prefix search]
   (when (= c search) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Public API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol PathFinder 
"An abstraction for path search within arbitrary data"
 (somep #_[this prefix search pred] [this search] 
 "Retrieves the first found path leading to a node with value matching search according to pred (defaults to =) ")
 (paths [this prefix] [this] "Yields a vector of all paths leading to leaf nodes and appends it to prefix (defaults to []")
 (get-at [this path]))

(def path-finder-with-indexes-impls {:somep some-path-using-indexes
                                     :paths paths-with-indexes
                                     :get-at get-with-indexes })
(def path-finder-with-keys-impls {:somep some-path-using-keys
                                  :paths paths-with-keys
                                  :get-at get-with-keys})

(def path-finder-with-values-impls {:somep some-path-using-values
                                    :paths paths-with-values
                                    :get-at get-with-keys})

(extend clojure.lang.IPersistentList
  PathFinder path-finder-with-indexes-impls)
(extend clojure.lang.IPersistentVector  
  PathFinder path-finder-with-indexes-impls)
(extend clojure.lang.IPersistentMap  
  PathFinder path-finder-with-keys-impls)
(extend clojure.lang.IPersistentSet
  PathFinder path-finder-with-values-impls)
(extend clojure.lang.Cons  
  PathFinder path-finder-with-indexes-impls)
(extend clojure.lang.ISeq   
  PathFinder path-finder-with-indexes-impls)
(extend-type java.lang.Object
  PathFinder
  (somep [this search]
   (when (= search this) ()))
  (paths 
    ([this prefix] prefix)
    ([this] []))
  (get-at [this path] 
    (throw (ex-info "Not a collection, or recognized as such" {:coll this :path path}))))
