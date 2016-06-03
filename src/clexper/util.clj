(ns clexper.util)

(defn occurs-ratio 
"Yields the number of occurrences of v in coll 
 divided by the size of coll
"
[coll v] 
  (->> coll 
       (filter #{v})
       count 
       (#(/ % (count coll)))))


(defn hist 
"Yields a histogram from a distribution of n samples.
 distribute-fn is a no-args fn yielding a fixed size collection 
 of the same values, possibly with different value distributions 
 each time.
"
[distribute-fn n] 
  (->> (range n) 
       (map (fn [_] (distribute-fn)))
       frequencies 
       (sort-by first)))


(defn padded-str [siz o]
  (let [s (str o) 
        padsiz (max 0 (- siz (count s)))]
    (-> (apply str (repeat padsiz " "))
        (str s))))


(defn print-hist [hist margin] 
  (doseq [h hist] 
    (println (padded-str margin (first h)) 
             (->> (repeat (last h) "*") 
                  (apply str)))))


;; test.check generator with frequency, example prodded with hist/print-hist:
;; 
#_(defn days [n ] (gen/sample (gen/frequency [[5 (gen/return :weekday)] [2 (gen/return :weekend)]]) n))
;; (print-hist (hist #(occurs-ratio (days 21) :weekend) 100) 5) 
