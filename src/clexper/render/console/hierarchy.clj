(ns clexper.render.console.hierarchy)

(def glyphs {:pipe "\u2502"
             :angle "\u2514"
             :wick "\u251C"
             :space " "})

(def endline "\n")

(def ^:dynamic *leaf-glyph-key* :angle)

(defn format->glyph [] {:continue :pipe
                        :none :space
                        :leaf *leaf-glyph-key*
                        :end-leaf :angle})


(defn prune-from [pairs children]
  (remove (fn [[c _]]
            ((set children) c)) 
          pairs))

(defn find-root [pairs]
  (some (fn [[c p]]
          (when (nil? p)
            c))
        pairs))

(defn pluck-children [pairs parent]
  (as-> pairs it
      (reduce (fn [children [c p]]
                (if (= p parent)
                  (conj children c)
                  children))
              []
              it)
      (if (seq it)
        [it (prune-from pairs it)]
        nil)))

(defn hline [stack]
  (vec (reverse stack)))


(defn args [m]
  (reduce (fn [acc kv]
            (into acc kv))
          [] m))

(defn expand-pairs
"Yields a vector of all hierarchy lines in rels. 
  rels is a vector of direct hierarchies [x y] where x is directly
  below y.

  A hierarchy line is a vector of elements where each element is under the 
  previous one (the first element reports to no other).

  The result contains all paths from root to leaf, such that all 
  children's paths follow immediately their parent. Otherwise the ordering 
  depends on the input's ordering.

  For example [[A] [A B C D] [A E] [A B F]] is not a valid output, since 
  F is cut-off from its parent B and siblings C and D. 
"
  ( [pairs [p & _ :as parents] out {:keys [compare-fn sorted?] :as opts}]
   (if (empty? parents)
     out
     (if-let [[children pruned-pairs] (seq (pluck-children pairs p))]
       (as-> children it
         (if sorted?
           (sort compare-fn it)
           it)
         (reduce (fn [lines c]
                   (let [stack (conj parents c)]
                     (as-> lines it
                       (conj it (hline stack))
                       (expand-pairs pruned-pairs stack it opts))))
                 out
                 it)
         (recur pairs (pop parents) it opts))
       (recur pairs (pop parents) out opts)))))


(defn expand 
  ([pairs compare-fn]
    (let [root (find-root pairs)]
     (expand-pairs pairs (list root) [[root]] 
                   {:sorted? true :compare-fn compare-fn})))
  ([pairs]
   (let [root (find-root pairs)]
     (expand-pairs pairs (list root) [[root]] {}))))

(defn prefixes [[x & xs]]
  (->> xs
       (reduce (fn [[head & pfx :as prefixes] x]
                 (conj prefixes (conj head x)))
               (list [x]))
       reverse
       butlast))


(defn format-data [line [nextline & _ :as nextlines] prevline]
  (let [pfx-map (zipmap line (prefixes line))]
    (->> 
     (map (fn [[k pfx]]
            (let [last-pfx? (= pfx (butlast line))
                  pfx-found-later? (some #(= pfx %)
                                         (map butlast nextlines))
                  pfx=prevline? (= pfx prevline)]
              [k, 
               (cond
                 (and last-pfx? 
                      (not pfx-found-later?))
                 :end-leaf
                 
                 (and last-pfx?
                      pfx-found-later?)
                 :leaf

                 pfx-found-later?
                 :continue
                 
                 :else
                 :none)]))
          pfx-map)
     (apply conj {}))))


(defn format-glyph [format-datum]
  (->> format-datum
       (get (format->glyph))
       (get glyphs)))


(defn format-line [pfx-data line]
  (let [[pfx token :as splitline] [(butlast line) (last line)]]
    (as-> 
        (mapv #(get pfx-data %)
              pfx) 
        it 
      (mapv format-glyph it)
      (conj it token)
      (apply str it))))


(defn lineout [appended line]
  (str appended line endline))


(defn render
  "Only the 1-arity should be invoked externally. 'lines is a
  vector of hierarchy lines as output by fn expand. 
  Yields an ASCII string for console rendering of the hierarchy."
  ([[line & lines] [prevline & _ :as prevlines] output]
   (cond
     (nil? line)
     output

     (nil? prevline)
     (->> (lineout output (first line))
          (recur lines 
                 (conj prevlines line)))

     :else
     (as-> (format-data line lines prevline)
         it
       (format-line it line)
       (lineout output it)
       (recur lines (conj prevlines line) it))))
  ([lines]
   (render lines () "")))



(defn hierarchy 
"Yields a ASCII string representation of the hierarchy encoded
by direct-reports with all elements directly or indirectly reporting
to a unique element, which reports to no one. 

The input is a sequence of duples [x y] such that x is directly under y 
if x is the root (reports to no one), y must be nil. 
"
  ([direct-reports use-wick? sort? compare-fn]
   (binding [*leaf-glyph-key* (if use-wick? :wick :angle)]
     (as-> direct-reports it
       #_(apply expand it (if sort? [compare-fn] [nil]))
       (if sort?
         (expand it compare-fn)
         (expand it))
         (render it))))
  ([direct-reports use-wick? sorted?]
   (hierarchy direct-reports use-wick? sorted? nil))
  ([direct-reports use-wick?]
   (hierarchy direct-reports use-wick? nil nil))
  ([direct-reports]
   (hierarchy direct-reports nil)))


(defn render-and-print 
"A convenience for quick testing at the REPL from within this 
 namespace."
  ([input sorted?]
   (println (hierarchy input true sorted? nil)))
  ([input]
   (render-and-print input false)))
