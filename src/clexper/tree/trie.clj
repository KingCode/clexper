(ns clexper.tree.trie
  (:require [clojure.zip :as zip]
            [clojure.string :as str]
            [clexper.render.console.hierarchy :as h]))

(defprotocol TrieOps  
  (search [_ path])
  (add [_ word]))

(defrecord Node [parent label value children])
(defrecord Trie [alphabet ^Node root index])

(defn path 
  ([^Node node p]
   (if node
     (recur (.parent node) (conj p (.label node)))
     p))
  ([^Node node]
   (path node nil)))

;;;;; pretty-printing utils
;;;;;;;;;;;;;;;;;;
(def ^:dynamic ^:private *tabs* -1)
(def tabsiz 2)
(def space " ")
(def ^:dynamic *show-orphans* false)

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
                                     :when (or c *show-orphans*)] 
                                 (if c 
                                   (str (pr-str c))
                                   (str "\n"(tabs+) "^"))))
                    ;; (tabs) "]"
                    "]")))))

(defmethod print-method Trie [v ^java.io.Writer w]
  (.write w (tabify 
             (str *ns* ".Trie[" :alphabet (.alphabet v) 
                  ;; ", :index " (.index v)
                  (tabs)
                  (pr-str (.root v))
                  "]"))))

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

(defn mark-word [^Node node]
  (make-node (.parent node) (.label node) ::word (.children node)))

(defn word? [^Node node]
  (identity (.value node)))
(defn marked-as-word? [^Node node]
  (= ::word (.value node)))
(defn word-mark? [x]
  (= ::word x))
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



#_(defn choice-fn [{:keys [index]} word-pred]
  (letfn 
      [(paths [^Node node path]
         (if (word-pred node)
           (conj path (.label node)))
         (for [c (.children from) :when c]
           (words (.label c))))
       (words [x ^Node from path]
         (cond 
           (nil? x)
           [path]
           (nil? from)
           nil
           :else 
           (if-let [child (select-child from index x)]
             
             )
           )
         )]) 
;; h  TRIE [] 
;;   [he hi heel hello]
;; e  node-1 [h]
;;   [he heel hell hello] [he]
;; l node-2 [he]
;;   [hell hello] [hel]
  
               
 )

(defn branch? [^Node node]
  (some identity (.children node)))


(defn paths-from [^Node node]
  (cond 
    (nil? node)
    []
    (branch? node)
    (for [k (.children node)
          :when k]
      (map (partial list* 
                     (if (word? node) 
                       [(.label node) ::branching-word]
                       (.label node)))
           (paths-from k)))
    :else
    [[(.label node) ::word]]))


(defn aggregate [wordline]
                     (reduce (fn [[w & ws :as words] x]
                               (if (= ::branching-word x)
                                 (cons w words)
                                 (cons (conj w x) ws)))
                             [[]]
                             wordline))


(defn paths [^Node node]
  "Yields all paths from 'node to its leaves, including 
word end marks."
  (let [part-fn #(or (nil? %)
                     (word-mark? %))]
    (->> node
         paths-from
         flatten
         (partition-by part-fn)
         (remove (comp part-fn first))
         (map aggregate)
         (apply concat)
         set)))

(defn all-paths [^Trie trie]
  (paths (.root trie)))


(defn select-child [^Node node index x]
  (when node
    (aget (.children node) (index x))))


(defn navigate [^Trie trie path]
  (reduce (fn [^Node node x]
            (select-child node (.index trie) x))
          (.root trie)
          path))


(defn choice-fn [^Trie trie]
  (fn [path x]
    (->>
     (select-child (navigate trie path)
                   (.index trie) 
                   x)
     paths
     (map (partial concat path))
     (map (partial apply str))
     sort)))


(defn make-ht []
  (let [ht (empty-trie "heloi")]
    (add-word ht (seq "eel"))
    (add-word ht (seq "hello"))
    (add-word ht (seq "hell"))
    (add-word ht (seq "he"))
    (add-word ht (seq "hi"))
    (add-word ht (seq "ho"))
    (add-word ht (seq "lie"))
    (add-word ht (seq "heel"))
    (add-word ht (seq "lee"))
    ht)
  )

