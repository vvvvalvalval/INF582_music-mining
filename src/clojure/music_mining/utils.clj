(ns music-mining.utils)

(defn alert-null [value message]
  (when (nil? value)
    (println "NIL ALERT : " message))
  value)

(def not-nil? (complement nil?))

(defn do-nothing 
  "Takes any arguments, does nothing with them and returns nil."
  [& args] nil)

(defn failing-to-nil
  "Wraps f in a function that takes the same arguments but returns nil when an Exception is thrown"
  ([f] (failing-to-nil f do-nothing))
  ([f do-when-Exception]
    (fn [& args]
      (try
        (apply f args)
        (catch Exception e (do-when-Exception args e)))))
  )


(defn take-sparsely [n coll]
  "Takes n elements from coll as separated as can be"
  (let [size (count coll)
        step (quot (+ size (- n (rem size n))) n)]
    (take-nth step coll)))

(defn time-sequence-traversal [coll]
  (let [count (atom 0)
        leap 250]
    (time
      (doseq [item coll]
        (swap! count inc)
        (when (= 0 (rem @count leap))
          (println "I have processed " @count " items so far"))))))


(defn sum 
  "Sums a sequence"
  [coll]
  (loop [remaining coll
         res 0]
    (if (empty? remaining)
      res
      (recur (rest remaining)
             (+ (first remaining) res)))))


(defn transform-map
  "Transforms a map into another map by applying the specified function to all values."
  [f mp]
  (loop [remaining-keys (keys mp)
         res {}]
    (if (empty? remaining-keys)
      res
      (recur (rest remaining-keys)
             (let [key (first remaining-keys)
                   new-value (f (mp key))]
               (assoc res key new-value))))))
      
    