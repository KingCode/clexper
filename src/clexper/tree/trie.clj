(ns clexper.tree.trie)

(defprotocol NodeOps
  (search [_ k])
  (add [_ k f])
  (prune [_ k f]))

(defrecord Node [parent label value children])

(defn make-node [^Node parent label value children]
  (->Node parent label value children))

(def NilNode (make-node nil nil nil nil))

(defn empty-children [n]
  (make-array (class NilNode) n))

(defrecord Trie [alphabet ^Node root index])

(defn make-root [arity]
            (make-node nil nil nil (empty-children arity)))

(defn empty-trie [alphabet]
  (let [alphabet (vec alphabet)] 
    (->Trie alphabet
            (make-root (count alphabet))
            (->> alphabet count range
                 (map (fn [v k] [v k])
                      alphabet)
                 (into {})))))

(defn valid? [alphabet word]
  (every? (set alphabet) word))

(defn validate [^Trie trie word]
  (assert (valid? (:alphabet trie) word)))


(defn set-child! [^Node parent idx ^Node child]
  (assert (< -1 idx (count (.children parent))))
  (aset (.children parent) idx child))


(defn update-branch [^Node node idx f]
  (set-child! (.parent node)
              idx
              (f node)))


(defn ^Node set-value [^Node node v]
  (make-node (.parent node) (.label node) v (.children node)))

(defn search-word 
"Yields a [path, node] if part or the whole of 'word is found in 'trie,
 where path is a prefix of 'word and node is the leaf node with the last 
 element of prefix; returns nil if no prefix is found.
The searched word must be valid."
[^Trie trie word]
  (validate trie word)
  (let [[path node]
        (reduce (fn [[path ^Node node] x]
                  (let [node' (aget (.children node) 
                                    ((.index trie) x))]
                    (if node' 
                      [(conj path x), node']
                      (reduced [path node]))))
                [[], (:root trie)]
                (seq word))]
    (when (seq path)
      [path node])))

;;
;; Utilities for using the trie to store words only (the default) 
;;
(def WordEndMark ::word)

(defn mark-word [^Node node]
  (make-node (.parent node) (.label node) WordEndMark (.children node)))

(defn word? [^Node node]
  (= WordEndMark (.value node)))
;;;;;;;;


;;
;; Utilities for both storing words and arbitrary values associated
;;
;; TODO


(defn add-word 
  "Adds nodes so that 'word is part of the node's paths. If provided,
  xnode is a function taking a node and returning a node marked as
  ending a word."
  ([^Trie trie word xnode]
   (validate trie word)
   (let [[path node] (or (search-word trie word)
                         [[],(.root trie)])
         vword (vec word)
         todo (subvec vword (count path))
         arity (count (.alphabet trie))]
     (->> todo 
          (reduce (fn [^Node node x]
                    (let [node' 
                          (make-node node x nil 
                                     (empty-children arity))]
                      (aset (.children node) 
                            ((.index trie) x)
                            node')
                      node'))
                  node)
          (#(update-branch %
            ((.index trie) (.label %))
            xnode)))))
  ([^Trie trie word]
   (add-word trie word mark-word)))


(defn paths-as-nested 
  ([^Node node pred]
   (cond 
     (nil? node)
     nil
     (some identity (.children node))
     (for [k (.children node) 
           :when (not (nil? k)) 
           :let [label (.label node) 
                 ps (paths-as-nested k)]]
       (if (pred node)
         (cons label (cons WordEndMark ps))
         (cons label ps)))
     (pred node)
     [(.label node) WordEndMark]
     :else
     [(.label node)]))
  ([^Node node]
   (paths-as-nested node word?)))


(defn paths [^Trie trie]
  (->> (.root trie) 
       paths-as-nested
       flatten
       (partition-by nil?)
       (filter #(identity (first %)))))


(defn expand [path]
  (first 
   (reduce (fn [[words add-word?] x]
             #_(println :WORDS words :ADD? add-word? :X x)
             (cond
               (empty? words) 
               [[[x]], false]
               
               add-word? 
               [(conj words 
                      (conj (last words) x)), false]

               (= WordEndMark x)
               [words, true]

               :else
               [(conj (vec (butlast words))
                      (conj (last words) x)), false]))
           [[], false]
           path)))

(defn expand-all [paths]
  (mapcat #(expand %) paths))

(defn make-ht []
  (let [ht (empty-trie "helo")]
    (add-word ht (seq "eel"))
    (add-word ht (seq "hello"))
    (add-word ht (seq "hell"))
    (add-word ht (seq "he"))
    ht))
