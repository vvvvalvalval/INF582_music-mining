(ns music-mining.core
  (:require [music-mining.data-imports :as di]
            [music-mining.distributions]
            [clojure.contrib.math :as math]
            clojure.set)
  (:import [music_mining.data_imports NiceSong NotesGroup]
           [music_mining.distributions WeightedPoint])
  (:use [clojure.repl]
        [clojure.pprint]
        [music-mining.utils]
        [music-mining.distributions]
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
        
(defn extract-distributions-per-track-and-merge
  "Defines a function that extracts a distribution from a song by extracting a distribution on the notes of each of its tracks, and summing the results."
  [extract-from-track-notes]
  (fn [^NiceSong song]
    (let [notes-of-tracks-seq (map :notes (:content song))]
      (reduce +distributions
              (map extract-from-track-notes notes-of-tracks-seq)))))

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

(let [to-steps-seq (fn self [notesGroups-seq]
                       (let [[{pos1 :position} {pos2 :position} & _] notesGroups-seq]
                         (if (nil? pos2)
                           ()
                           (lazy-seq
                             (cons (- pos2 pos1) (self (rest notesGroups-seq)))))))
      extract-from-track-notes (comp aggregate-into-distribution to-steps-seq)
      extract-unnormalized (extract-distributions-per-track-and-merge extract-from-track-notes)
      normalize (fn [unnormalized-distrib]
                  (let [{most-frequent-step :point} (maximal-point-info unnormalized-distrib)]
                    (if (nil? most-frequent-step)
                      {} ;there is no step!
                      (image-distribution #(/ % most-frequent-step) ;normalizing 
                                          unnormalized-distrib))))]
  
  (defn rhythm-steps-distribution-of-song
    "Extracts from a song the distribution of steps, that is the duration between successive notes.
Returns a normalized distribution so the most frequent step has duration 1."
    [^NiceSong song]
    (->> song extract-unnormalized normalize)))
      
(let [noteGroup-to-interval (fn [{pitches :pitches}]
                              (let [sorted-pitches (sort pitches)
                                    min-pitch (first sorted-pitches)]
                                (->> pitches (map #(- % min-pitch)) rest vec)))
      
      polyphonic? (fn [{pitches :pitches}] (> (count pitches) 1))
      extract-from-track-notes (fn [track-notes] (->> track-notes 
                                                   (filter polyphonic?)
                                                   (aggregate-into-distribution (fn [noteGroup]
                                                                                  {:point (noteGroup-to-interval noteGroup)
                                                                                   :weight (:duration noteGroup)}))))]
  
  (def intervals-distributions-of-song
    " [song]
Extracts from a song the distribution of intervals"
    (extract-distributions-per-track-and-merge extract-from-track-notes)))
      
(defrecord 

                                                                                
                                                   
    
        
        
                             
  
  
  
  
  
  
  
  
  
  
  
  
    