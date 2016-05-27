(ns clexper.kpath)

(declare key-paths)

(defn show-unwrap [all paths p] 
  (println (str "UNWRAP (ALL=" all ", PATHS=" paths ", P=" p)))

(defn show-subpaths [k v prefix kps] 
  (println (str "SUBPATHs (" "K=" k ", V=" v ", PFX=" prefix "\n\tKPATHS for v=" kps)))

(defn subpaths [k v prefix]               
  (let [subp (key-paths v)]
    #_(show-subpaths k v prefix subp)
    (if (seq subp)
      (->> subp
           (mapv #(apply conj prefix k %)))
      (conj prefix k))))

(defn unwrap [paths]
  (let [raw (reduce 
             (fn [ps p]
               #_(show-unwrap paths ps p)
               (into ps p))
             [] 
             paths)]
    ;;Wrap back 'naked' singleton (top-level) paths
    ;;for uniformity
    (mapv (fn [p]
            (if-not (coll? p) 
              [p]
              p)) 
          raw)))


(defn paths-with-indexes [c prefix]
  (->> c
       (mapv (fn [i e]
               (subpaths i e prefix))
             (range))
       unwrap))
  

(defn paths-with-keys [c prefix]
  (->> c
       (mapv (fn [[k v]] 
               (subpaths k v prefix)))
       unwrap))
  

(defn paths-with-values [c prefix]
  (->> c 
       (mapv (fn [v]
               (subpaths v v prefix)))
       unwrap))


(defmulti key-paths (fn dispatch 
                      ([c _] (dispatch c))
                      ([c] 
                       (cond (seq? c) :seq
                             (map? c) :map
                             (associative? c) :associative
                             (sequential? c) :sequential
                             (set? c) :set
                             :else nil))))


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

  
