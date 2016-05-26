(ns clexper.kpath)

(declare key-paths)


(defn subpaths [k v prefix]               
  (let [subp (key-paths v)]
    (if (seq subp)
      (->> subp
           (mapv #(apply conj prefix k %)))
      (conj prefix k))))

(defn unwrap [paths]
  (reduce (fn [ps p]
            (if (< 1 (count p))
              (into ps p)
              (conj ps p)))
          [] 
          paths))


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

(comment
"Given any collection instance, we want to extract its navigational structure,
 i.e. all key sequences leading to a leaf node in post-order sequence.

 For example, 
        (key-paths '(1 {:a [2 3 {4 :four}] :b nil} #{\\A {:c :whathever}}))
 should yield:
       ;;=> ([0] [1 :a 0} [1 :a 1] [1 :a 2] [1 :a 2 4] [1 :b] [2])

")

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

  