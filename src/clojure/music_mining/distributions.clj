(ns music-mining.distributions
  (:use [clojure.repl]
        [clojure.pprint]
        [music-mining.utils])
  (:require [clojure.contrib.math :as math]
            clojure.set))


(def zero-distribution 
  "A distribution with only zero values."
  {})

(defn image-distribution
  "Transforms a distribution (a map with numeric values) into the image distribution by the specified transform of the keys."
  [transform initial-distibution]
  (reduce (fn [distribution key]
            (let [image (transform key)
                  old-value (or (distribution image) 0)]
              (assoc distribution image (+ old-value (initial-distibution key)))))
          {}
          (keys initial-distibution)))

(defrecord WeightedPoint [point weight])

(let [counter-fun (fn [item] (->WeightedPoint item 1))]
  (defn aggregate-into-distribution
    ([adder-fun coll]
    "Aggregates the provided sequence into a distributions, typically a distributions counting the occurences of each item in the coll.
The optional adder-fun argument is a function that takes a collection item and extracts from it a :point and a :weight, and returns them in a map, for example as a WeightedPoint."
    (reduce (fn [bag item]
              (let [{point :point
                     weight :weight} (adder-fun item)]
                (assoc bag point (+ weight (or (bag point) 0)))))
            {}
            coll))
    ([coll] (aggregate-into-distribution counter-fun coll))))

(defn distributions-binary-operator
  "Creates a binary operator for distributions from the provided per-value-operator.
The resulting distribution is obtained by applying per-value-iterator on a pointwise basis.
When a point is not explicitly represented in a distribution, it is defaults to the provided default value (or 0 if not specified, which is the non-degenerate behavior."
  ([per-value-operator default-value-1 default-value-2]
    (fn dbo
      ([] {})
      ([d] d)
      ([d1 d2]
        (reduce (fn [distrib point]
                  (let [op-value (per-value-operator (or (d1 point) default-value-1) (or (d2 point) default-value-2))]
                    (if (= op-value 0)
                      distrib
                      (assoc distrib point op-value))))
                {}
                (clojure.set/union (keys d1) (keys d2))))
      ([d1 d2 & rest]
        (reduce dbo (dbo d1 d2) rest))))
  ([per-value-operator default-value]
    (distributions-binary-operator per-value-operator default-value default-value))
  ([per-value-operator] (distributions-binary-operator per-value-operator 0 0)))

(def +distributions (distributions-binary-operator +))
(def -distributions (distributions-binary-operator -))
(def *distributions (distributions-binary-operator *))

(defn dotp-distributions
  [distrib-1 distrib-2]
  (sum (vals *distributions distrib-1 distrib-2)))

(defn norm2-distributions
  [distrib]
  (dotp-distributions distrib distrib))

(defn distance-euclidean-distributions
  [distrib-1 distrib-2]
  (let [difference (-distributions distrib-1 distrib-2)]
    (math/sqrt (norm2-distributions difference))))

(defn cosine-similarity
  [distrib1 distrib2]
  (/ (dotp-distributions distrib1 distrib2)
     (math/sqrt (norm2-distributions distrib1))
     (math/sqrt (norm2-distributions distrib2))))
                

(defn maximal-point-info
  "Returns a map associated the highest value of the distribution to :value, and the first point to achive this value as :point"
  [distribution]
  (let [ks (keys distribution)]
    (if (empty? ks)
      {:value 0}
      (let [[first-point & others] ks]
        (reduce (fn [{best-value :value :as last-info} point]
                  (let [new-value (distribution point)]
                    (if (> new-value best-value)
                      {:point point
                       :value new-value}
                      last-info)))
                {:point first-point
                 :value (distribution first-point)}
                others)))))
                