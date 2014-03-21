(ns music-mining.core
  (:import (java.io InputStream InputStreamReader BufferedReader)
           (org.herac.tuxguitar.io.gtp GP1InputStream 
                                       GP2InputStream
                                       GP3InputStream
                                       GP4InputStream
                                       GP5InputStream
                                       GTPSettings)
           (edu.polytechnique.music_mining.tgmodels Beat
                                                    Duration
                                                    GString
                                                    Measure
                                                    Note
                                                    Song
                                                    Track
                                                    Voice)
           (org.herac.tuxguitar.io.tg TGInputStream)
           (org.herac.tuxguitar.song.models TGBeat
                                            TGDivisionType
                                            TGDuration
                                            TGMeasure
                                            TGNote
                                            TGNoteEffect
                                            TGSong
                                            TGString
                                            TGVoice
                                            TGTrack)
           (org.herac.tuxguitar.song.models.effects TGEffectBend
                                                    TGEffectTremoloBar)
           (org.herac.tuxguitar.song.factory TGFactory)
           )
  (:require [clojure.java.io :as cio]
            )
  (:use clojure.repl
        clojure.pprint
        music-mining.utils)
  )

(defn resource-as-stream 
  "loads a classpath resource as an (open, be careful!) stream."
  [resource-name]
  (. (cio/resource resource-name) openStream))

; creating setting for GTP readers
(def settings (GTPSettings.))
; the factory that is used to create songs and other TG models. Overriden to have more readable objets.
(def tg-factory 
  "A special TGFactory implementation that lets us define our own types for constructing TGSong, notably with enhanced toString method."
  (proxy [TGFactory] []
    (newBeat [] (Beat. tg-factory))
    (newDuration [] (Duration. tg-factory))
    (newMeasure [header] (Measure. header))
    (newNote [] (Note. tg-factory))
    (newSong [] (Song. ))
    (newString [] (GString. ))
    (newTrack [] (Track. tg-factory))
    (newVoice [index] (Voice. tg-factory index))
    ))
    

(let [GPInputStream-factory-method-for-format {:gp1 (fn [] (GP1InputStream. settings)),
                                               :gp2 (fn [] (GP2InputStream. settings)),
                                               :gp3 (fn [] (GP3InputStream. settings)),
                                               :gp4 (fn [] (GP4InputStream. settings)),
                                               :gp5 (fn [] (GP5InputStream. settings))
                                               ;:tg (fn [] (TGInputStream.))
                                               }
      new-GPInputStream (fn [format] ((GPInputStream-factory-method-for-format format)))]
  (defn ^TGSong read-song
    "Reads a song of the specified format from the provided InputStream"
    [input-stream, format]
    (let [gpis (new-GPInputStream format)]
      (. gpis (init tg-factory input-stream))
      (. gpis (readSong)))
    ))

;some sample songs
(def chello (read-song (resource-as-stream "sample-gp3.gp3") :gp3))
(def rhapsody (read-song (resource-as-stream "rhapsody.gp4") :gp4))
(def au-clair-de-la-lune (read-song (resource-as-stream "au_clair_de_la_lune.gp4") :gp4))
(def drums (read-song (resource-as-stream "drums.gp4") :gp4))
(def several-instruments (read-song (resource-as-stream "several_instruments.gp4") :gp4))
(def two-voices (read-song (resource-as-stream "2_voices.gp4") :gp4))
(def smoke-water (read-song (resource-as-stream "smoke_on_the_water.gp3") :gp3))
(def 
  bend (read-song (resource-as-stream "bend.gp4") :gp4))
(def all-samples [chello 
                  rhapsody 
                  au-clair-de-la-lune 
                  drums 
                  several-instruments
                  two-voices
                  smoke-water])

(defn percussion-track?
  "Whether a track is a percussion track"
  [^TGTrack track]
  ;we know it's a percussion track because the value of a string is 0. Very ugly, but that's how its programmed.
  (= 0 (.. track (getString 1) (getValue))))

; Represents a set of notes played on a instrument at a point in time, which have the same duration.
(defrecord NotesGroup [position 
                       duration 
                       pitches])

(defn to-ratio-duration 
  "Converts a TGDuration to a rational number, where the duration of a quarter note is 1."
  [^TGDuration tg-duration]
  (*
    (/ 4 (. tg-duration getValue)) ; a 4th has value 4
    (if (. tg-duration isDotted) ; acconting for dots
      3/2
      (if (. tg-duration isDoubleDotted) 7/4 1))
    (let [^TGDivisionType division-type (. tg-duration getDivision)] ; accounting for division type
      (/ (. division-type getTimes) (. division-type getEnters)))
    ))

(def reference-pitch-value 
  "The TuxGuitar value of the origin pitch."
  ;that's a C5.
  60)
(defn ^long pitch-of-fretted-note
  "Computes a pitch from a TGString and a fret number."
  [^TGString tg-string
   ^TGNote note]
  (let [fret-number (. note getValue)
        ^TGNoteEffect effects (. note getEffect)
        ^TGEffectBend bend-effect (. effects getBend)
        ^TGEffectTremoloBar tremolo-bar-effect (. effects getTremoloBar)]
    (+  (. tg-string getValue) ; the pitch of the open string
        fret-number ; the number of the fret
        (if bend-effect ; adding any bend effect, the final value is used
          (let [points (. bend-effect getPoints)]
            (if (empty? points)
              0
              (quot (. (last points) getValue) 2)))
          0)
        (if tremolo-bar-effect ; adding any tremolo bar effect, the final value is used
        (let [points (. tremolo-bar-effect getPoints)]
          (if (empty? points)
            0
            (quot (. (last points) getValue) 2)))
        0)
        (- reference-pitch-value) ; substracting the reference pitch.
        )))

(defn notes-of-track
 "Transform a TGTrack into a sequence of NoteGroups." 
 [^TGTrack tg-track]                      
 (let [tgstring-with-number (fn [number] (.. tg-track (getString number))),
       measures (iterator-seq (.. tg-track (getMeasures))),
       find-notes-in-measure (fn [measure initial-offset]
                              (loop [remaining-beats (. measure getBeats),
                                     position initial-offset,
                                     notes-in-measure []]
                                ;looping through each beat in the measure
                                (if (empty? remaining-beats)
                                    {:final-offset position,
                                     :notes notes-in-measure}
                                    (let [current-beat (first remaining-beats),
                                          ;in Guitar Pro files parsed with TuxGuitar, only the first voice ever gets any note. YES, this is dirty.
                                          voice (. current-beat (getVoice 0)),
                                          duration (to-ratio-duration (. voice getDuration)),
                                          pitches (vec (map 
                                                         (fn [^TGNote tg-note]
                                                           ;we transform the TGNote into an integer-valued pitch
                                                           (pitch-of-fretted-note
                                                             (tgstring-with-number (. tg-note getString))
                                                             tg-note))
                                                         (. voice getNotes))) 
                                          ]
                                      (recur (rest remaining-beats)
                                              (+ position duration)
                                              (conj notes-in-measure (->NotesGroup position duration pitches))))
                                   )))]   
   (loop [remaining-measures measures
         position-offset 0
         notes []]
     ;looping through all measures
    (if (empty? remaining-measures)
       (filter (fn [^NotesGroup notes-group] (not-empty (:pitches notes-group))) notes)
     (let [current-measure (first remaining-measures)
           {new-offset :final-offset,
            notes-in-measure :notes} (find-notes-in-measure current-measure position-offset)]
       (recur (rest remaining-measures)
               new-offset
               (concat notes notes-in-measure)))))))

(defn notes-of-song
  [^TGSong song]
  "
Reads a song as a list of independent sequences of NoteGroups, one such sequence for each track.
The percussion track will not be accounted for, nor silent notes.
Note that the song is no longer partitioned in measures."
  (vec (map (fn [^TGTrack track]
              {:track-name (. track getName)
               :notes (notes-of-track track)})
            (filter (comp not percussion-track?) (iterator-seq (. song getTracks))))))
        
(defn pprint-song
  [^TGSong song]
  (pprint (notes-of-song song)))
  
          
          
            
      
       
        
        
      
      
          





