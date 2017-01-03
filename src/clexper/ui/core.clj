(ns clexper.ui.core
  (:require [clexper.ui.model :refer :all]))

(defn make-command 
  ([trigger?-fn, valid?-fn, target-fn, error-msg, opts]
   (Command-> trigger?-fn valid?-fn target-fn error-msg opts))
  ([trigger-val, target-fn, opts]
   (make-command #(= trigger-val %), (constantly true), target-fn, nil, opts))
  ([trigger-val, target-fn]
   (make-command trigger-val, target-fn, {})))


(defn get-commands 
"Retrieves n commands "
[^Menu menu, n])

