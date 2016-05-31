(ns clexper.ui.model)


(defrecord Command [
             trigger?-fn ;;a 1-arity (other than 'this) taking parsed input, 
                         ;;yields true if argument is intended for this command

             valid?-fn  ;;a 1-arity fn taking opts field to do further validation

             target-fn  ;;a 0-arity fn to be invoked as the result of the command  

             error-msg  ;;error to be used if (valid?-fn opts) is falsy
                    
             opts       ;;a map of options for user-specific purposes, passed to 
                        ;;valid?-fn
                    ])



(defrecord MenuItem [
       ^Command command ;; the wrapped command

       ^String label    ;; A short-form description of the command 

       ^String description ;;A long-form/addition description
                     ])

(defprotocol InputProcessor
  "An abstraction for input processing." 
  (parse [this input] "Yields a Command from parsing the input"))


(defprotocol InputSource
  "An abstraction which provides raw input"
  (next-input [this])
)


(defrecord Menu [menu-items, not-found-msg, input-source, input->trigger-fn]
  
  InputProcessor
  (parse [_ input]
    (let [trigger (input->trigger-fn input)]
      (some #(when ((:trigger?-fn (:command  %)) trigger) %)
            menu-items))))
    

(defn make-menu [])


