(ns music-mining.core
  (:require [music-mining.data-imports :as di]
            [clojure.contrib.math :as math]
            clojure.set)
  (:import [music_mining.data_imports NiceSong NotesGroup])
  (:use [clojure.repl]
        [clojure.pprint]
        [music-mining.utils]
        [music-mining.data-imports :only [chello rhapsody smoke-water au-clair-de-la-lune several-instruments
                                          reference-pitch-value]]))

(def fetch-songs di/fetch-a-few-songs)
(def fetch-all-songs di/fetch-all-songs)

(defn scale-note-of-pitch
  [pitch]
  (mod pitch 12))

(defn value-at-note [this pitch]
    (let [scale-note (scale-note-of-pitch pitch)]
      (if (contains? this scale-note)
        (this (scale-note-of-pitch pitch))
        0)))

(defn assoc-note [m-vector pitch value]
  (assoc m-vector (scale-note-of-pitch pitch) value))

(defn add-at-note
  "Adds the specified value at the specified pitch to the MVector"
  [m-vector
   pitch
   value]
  (let [old-value (value-at-note m-vector pitch)]
    (assoc-note m-vector pitch (+ old-value value))))
        

(defn total-time-per-pitch
  "Computes a map of the total duration that is spent playing each pitch.
Useful for having a 'harmonic distribution' of a song."
  [^NiceSong song]
  (let [process-NotesGroup (fn [duration-map
                                ^NotesGroup notes-group]
                             (let [{duration :duration
                                    pitches :pitches} notes-group]
                                (reduce (fn [duration-map
                                             pitch]
                                          (let [old-duration (get duration-map pitch 0)]
                                            (assoc duration-map pitch (+ old-duration duration))))
                                        duration-map
                                        pitches)))
        process-track (fn [duration-map
                           track]
                        (let [{track-notes :notes} track]
                          (reduce process-NotesGroup
                                  duration-map
                                  track-notes)))
        merge-duration-maps (fn [m1 m2] (merge-with + m1 m2))
        {song-content :content} song]
    (reduce merge-duration-maps
            {}
            (map #(process-track {} %) song-content))))

(defn normalize-distribution-map
  "Normalizes a map with numeric values so that the sum of the values is 1."
  [distribution-map]
  (let [total-weight (sum (vals distribution-map))]
    (if (zero? total-weight)
      (zipmap (range 0 12) (repeat 12 1/12))
      (transform-map #(/ % total-weight 1.0) distribution-map))))
    
(def pitch-time-distribution 
  "Computes a map representing the time distribution of each pitch."
  (comp normalize-distribution-map total-time-per-pitch))

(def scale-notes-seq (range 0 12))

(def zero-distribution 
  "A distribution with only zero values."
  {})
             
(def literal-note-of-scale-note {0 :C,
                                 1 :C#,
                                 2 :D,
                                 3 :D#,
                                 4 :E,
                                 5 :F,
                                 6 :F#,
                                 7 :G,
                                 8 :G#,
                                 9 :A,
                                 10 :Bb,
                                 11 :B})

  
(defn image-distribution
  "Transforms a distribution (a map with numeric values) into the image distribution by the specified transform of the keys."
  [transform initial-distibution]
  (reduce (fn [distribution key]
            (let [image (transform key)
                  old-value (or (distribution image) 0)]
              (assoc distribution image (+ old-value (initial-distibution key)))))
          {}
          (keys initial-distibution)))
                 
(def to-scale-notes-distribution 
  "Transforms a Pitch Distribution into a Scale Notes Distribution, by conserving total measure."
  (partial image-distribution scale-note-of-pitch))

(defn to-literal-distribution
  [scale-note-distribution]
  (image-distribution literal-note-of-scale-note scale-note-distribution))
              

(defn fundamental-note-evidence
  [snd]
  (let [most-frequent-scale-note (apply max-key snd scale-notes-seq)
        highest-weight (snd most-frequent-scale-note)
        without-most-frequent (dissoc snd most-frequent-scale-note)
        second-weight (apply max (vals without-most-frequent))]
    (if (zero? highest-weight) 0
      (/ (- highest-weight second-weight) 1))))

(def wellformedness (comp fundamental-note-evidence to-scale-notes-distribution pitch-time-distribution))

(defn shift-SND
  [snd shift]
  (reduce (fn [res note]
            (assoc-note res (+ note shift) (snd note)))
          {}
          (keys snd)))

(defn dot-product-SND
  "Natural dot product for Scale Note Ditributions"
  [snd1 snd2]
    (reduce (fn [sum pitch]
              (* (value-at-note snd1 pitch) (value-at-note snd2 pitch)))
            0
            (keys snd1)))

(defn +SND
  [snd1 snd2]
  (reduce (fn [res pitch]
            (add-at-note res pitch (snd2 pitch)))
          snd1
          (keys snd2)))

(defn -SND
  [snd1 snd2]
  (reduce (fn [res pitch]
              (add-at-note res pitch (- (snd2 pitch))))
            snd1
            (keys snd2)))

(defn scale-SND
  [coeff snd]
  (transform-map #(* coeff %) snd))

(defn euclidean-SND
  "Natural Euclidean Distance for Scale Note Ditributions"
  [snd1 snd2]
  (let [difference (-SND snd1 snd2)]
    (math/sqrt (dot-product-SND difference difference))))


(defn match-SNDs
  ""
  ([snd1 snd2 distance]
  (let [first-match (distance snd1 snd2)]
    (loop [best-match first-match
           best-shift 0
           shift 1]
      (if (= 12 shift)
        {:match best-match
         :shift best-shift}
        (let [new-match (distance snd1 (shift-SND snd2 shift))
              beaten (< new-match best-match)]
          (recur (if beaten new-match best-match)
                 (if beaten shift best-shift)
                 (inc shift)))))))
  ([snd1 snd2] (match-SNDs snd1 snd2 euclidean-SND)))


(defn height-zone [pitch]
  "Classifies the pitch as being :low, :medium or :high"
  (cond
    (< pitch -12) :low
    (> pitch 9) :high
    :else :medium))

(defn to-height-zone-distibution
  [pitch-time-distrib]
  (image-distribution height-zone pitch-time-distrib))

(defrecord WeightedPoint [point weight])

(let [counter-fun (fn [item] (->WeightedPoint item 1))]
  (defn aggregate-into-distribution
    ([adder-fun coll]
    "Computes a bag representation of the coll, that is a map mapping each item in the coll to the number of times it appears in the coll.
The optional adder-fun argument is a function that takes a collection item and extracts from it a :point and a :weight, and returns them in a map, for example as a WeightedPoint."
    (reduce (fn [bag item]
              (let [{point :point
                     weight :weight} (adder-fun item)]
                (assoc bag point (+ weight (or (bag point) 0)))))
            {}
            coll))
    ([coll] (aggregate-into-distribution counter-fun coll))))

(defn distributions-binary-operator
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

(def +distributions (distributions-binary-operator + 0))
(def -distributions (distributions-binary-operator - 0))
(def *distributions (distributions-binary-operator * 0))
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
                
                    

(defn extract-rythm-steps-distribution
  "Extracts from a song the distribution of steps, that is the duration between successive notes.
Returns a normalized distribution so the most frequent step has duration 1."
  [^NiceSong song]
  (let [to-steps-seq (fn self [notesGroups-seq]
                       (let [[{pos1 :position} {pos2 :position} & _] notesGroups-seq]
                         (if (nil? pos2)
                           ()
                           (lazy-seq
                             (cons (- pos2 pos1) (self (rest notesGroups-seq)))))))
        notes-of-tracks-seq (map :notes (:content song))
        unnormalized-distrib (reduce +distributions
                                     (map (comp aggregate-into-distribution to-steps-seq) notes-of-tracks-seq))
        {most-frequent-step :point} (maximal-point-info unnormalized-distrib)]
    (if (nil? most-frequent-step)
      {} ;there is no step!
      (image-distribution #(/ % most-frequent-step) ;normalizing 
                          unnormalized-distrib))))
      
    
    
        
        
                             
  
  
  
  
  
  
  
  
  
  
  
  
    