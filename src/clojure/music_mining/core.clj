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
           (org.herac.tuxguitar.song.models TGSong
                                            TGTrack)
           (org.herac.tuxguitar.song.factory TGFactory)
           )
  (:require [clojure.java.io :as cio]
            )
  (:use clojure.repl)
  )

(defn resource-as-stream 
  "loads a classpath resource as an (open, be careful!) stream."
  [resource-name]
  (. (cio/resource resource-name) openStream))

; creating setting for GTP readers
(def settings (GTPSettings.))
; the factory that is used to create songs and other TG models. Overriden to have more readable objets.
(def tg-factory 
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
                                               :gp5 (fn [] (GP5InputStream. settings)),
                                               :tg (fn [] (TGInputStream.))}
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
(def allsamples [chello rhapsody au-clair-de-la-lune drums several-instruments])

(defn percussion-track?
  "Whether a track is a percussion track"
  [^TGTrack track]
  ;we know it's a percussion track because the value of a string is 0
  (= 0 (.. track (getString 1) (getValue))))




