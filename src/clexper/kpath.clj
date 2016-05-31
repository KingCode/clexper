(ns clexper.kpath)

(declare key-paths)

(defn show-unwrap [all paths p] 
  (println (str "UNWRAP (ALL=" all ", PATHS=" paths ", P=" p)))

(defn show-subpaths [k v prefix kps] 
  (println (str "SUBPATHs (" "K=" k ", V=" v ", PFX=" prefix "\n\tKPATHS for v=" kps)))

(defn subpaths [k v prefix]               
  (let [subp (key-paths v)]
    (if (seq subp)
      (->> subp
           (mapv #(apply conj prefix k %)))
      (conj prefix [k]))))

(defn unpack [packed-paths]
  (reduce #(into % %2) [] packed-paths))

(defn paths-with-indexes [c prefix]
  (->> c
       (mapv (fn [i e]
               (subpaths i e prefix))
             (range))
       unpack))

(defn paths-with-keys [c prefix]
  (->> c
       (mapv (fn [[k v]] 
               (subpaths k v prefix)))
       unpack))
  

(defn paths-with-values [c prefix]
  (->> c 
       (mapv (fn [v]
               (subpaths v v prefix)))
       unpack))


(defn coll-category-dispatch 
  ([c _] (coll-category-dispatch c))
  ([c] 
   (cond (seq? c) :seq
         (map? c) :map
         (associative? c) :associative
         (sequential? c) :sequential
         (set? c) :set
         :else nil)))

(defmulti key-paths coll-category-dispatch)


(defmethod key-paths :seq
  ([c prefix]
   (paths-with-indexes c prefix))
  ([c]
   (key-paths c [])))


(defmethod key-paths :map
  ([c prefix]
   (paths-with-keys c prefix))
  ([c]
   (key-paths c [])))

(defmethod key-paths :associative
  ([c prefix]
   (paths-with-indexes c prefix))
  ([c]
   (key-paths c [])))

(defmethod key-paths :sequential 
  ([c prefix]
   (paths-with-indexes c prefix))
  ([c]
   (key-paths c [])))

(defmethod key-paths :set
  ([c prefix] 
   (paths-with-values c prefix))
  ([c]
   (key-paths c [])))

(defmethod key-paths nil
  ([c prefix] prefix)
  ([c] []))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Retrieving values from key paths           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


(defmulti get-at coll-category-dispatch)

(defmethod get-at :seq
  [c ks]
  (get-with-indexes c ks))

(defmethod get-at :map
  [c ks]
  (get-with-keys c ks))

(defmethod get-at :associative
  [c ks]
  (get-with-keys c ks))

(defmethod get-at :sequential
  [c ks]
  (get-with-indexes c ks))        

(defmethod get-at :set
  [c ks]
  (get-with-keys c ks))


(defmethod get-at nil
  [c ks]
  (throw (ex-info "Not a collection, or recognized as such" {:coll c :key-path ks})))
