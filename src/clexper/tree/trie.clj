(ns clexper.tree.trie
  (:require [clojure.zip :as zip]
            [clojure.string :as str]
            [clexper.render.console.hierarchy :as h]))

(defprotocol  
  ;; (search [_ k])
  (path [_]))

(defrecord Node [parent label value children])
(defrecord Trie [alphabet ^Node root index])

(defmethod print-method Trie [v ^java.io.Writer w]
  (.write w (tabify 
             (str *ns* ".Trie[" :alphabet (.alphabet v) 
                  ;; ", :index " (.index v)
                  (tabs)
                  (pr-str (.root v))
                  "]"))))

(def ^:dynamic ^:private *tabs* -1)
(def tabsiz 2)
(def space " ")

(defmacro tabify [ & body]
  `(binding [*tabs* (inc *tabs*)]
     ~@body))

(defn tabs 
  ([n]
   (apply str 
          (repeat (* n tabsiz) space)))
  ([]
   (tabs *tabs*)))

(defn tabs+ []
  (tabs (inc *tabs*)))


(defn node-mark [^Node node word-pred]
  (when node
    (str (.label node)
         (when (word-pred node)
           "*"))))


(defmethod print-method Node [v ^java.io.Writer w]
  (let [path (-> (loop [path () ^Node x v] 
                    (if (nil? x) 
                      path
                      (recur (conj path (.label x)) (.parent x))))
                 rest
                 (conj (str *ns* ".Node[" :path " |")))]
    (.write w (tabify 
               (str "\n"(tabs) 
                    (str/join "->-" path )
                    ",:value " (or (.value v) ::nil) 
                    (apply str (for [c (.children v)
                                     :when c] 
                                 (if c 
                                   (str (pr-str c))
                                   (str "\n"(tabs+) "^"))))
                    ;; (tabs) "]"
                    "]")))))


(defn make-node [^Node parent label value children]

  (->Node parent label value children))

(defn node-zipper [^Node root] 
  (zip/zipper (fn [^Node node]
                (some identity (.children node)))
              (fn [^Node node]
                (seq (.children node)))
              (fn [^Node node children]
                (make-node (.parent node)
                           (.label node)
                           (.value node)
                           children))
              root))

(def NilNode (make-node nil nil nil nil))

(defn empty-children [n]
  (make-array (class NilNode) n))


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
  (identity (.value node)))
#_(defn word? [^Node node]
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
         (list* label WordEndMark ps)
         (list* label ps)
         #_(cons label (cons WordEndMark ps))
         #_(cons label ps)))
     (pred node)
     [(.label node) WordEndMark]
     :else
     [(.label node)]))
  ([^Node node]
   (paths-as-nested node word?)))


(defn paths [^Node node]
  "Yields all paths from 'node to its leaves, including 
word end marks."
  (->> node
       paths-as-nested
       flatten
       (partition-by nil?)
       (filter #(identity (first %)))))

(defn all-paths [^Trie trie]
  (paths (.root trie)))

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

(defn narrow [pfx-paths ^Node branch x]
  (->> pfx-paths
         (filter #(= x (last %))))
  )


(defn make-ht []
  (let [ht (empty-trie "heloi")]
    (add-word ht (seq "eel"))
    (add-word ht (seq "hello"))
    (add-word ht (seq "hell"))
    (add-word ht (seq "he"))
    (add-word ht (seq "hi"))
    ;; (add-word ht (seq "ho"))
    ;; (add-word ht (seq "lie"))
    ;; (add-word ht (seq "heel"))
    ;; (add-word ht (seq "lee"))
    ht)
  )

