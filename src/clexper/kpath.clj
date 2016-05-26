(ns clexper.kpath)

#_(deftype SetSubpathFn [s]
  clojure.lang.IFn
  (invoke [_ k] (s k)))
(declare key-paths)

(defn paths-with-indexes [c prefix]
  (->> c
       (mapv (fn [i e]
               (let [subp (key-paths e)]
                 (if (seq subp)
                   (->> subp
                        (mapv #(apply conj prefix i %)))
                   (conj prefix i))))
             (range))
       (reduce (fn [ps p]
                 (if (< 1 (count p))
                   ;;hoist composite paths onto top level                  
                   (into ps p) 
                   (conj ps p)))
               [])))
  
(defn paths-with-keys [c prefix]
  (mapv (fn [[k v]] 
          (apply conj prefix k (key-paths v)))
        c))
  

(defn paths-with-values [c prefix]
  (mapv (fn [v]
          (apply conj prefix v (key-paths v)))))

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

  
