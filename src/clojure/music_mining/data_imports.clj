(ns music-mining.data-imports
  (:import (java.io InputStream InputStreamReader BufferedReader File)
           (org.apache.commons.io FilenameUtils)
           (org.herac.tuxguitar.io.gtp GP1InputStream 
                                       GP2InputStream
                                       GP3InputStream
                                       GP4InputStream
                                       GP5InputStream
                                       GTPInputStream
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

; Some important constants for the project.
(def reference-pitch-value 
  "The TuxGuitar value of the origin pitch."
  ;that's a C5.
  60)
(def all-songs-directory
  "The folder containing all the songs to be processed"
  (cio/file "/home/val/Bureau/Cours" "INF Datamining" "projet_music" "tabs"))
(def development-songs-directory
  "The folder containing a smaller set of songs, to use for tests in development."
  (cio/file "/home/val/Bureau/Cours" "INF Datamining" "projet_music" "development_songs"))


(defn- resource-as-stream 
  "loads a classpath resource as an (open, be careful!) stream."
  [resource-name]
  (. (cio/resource resource-name) openStream))

; creating setting for GTP readers
(def ^GTPSettings settings (GTPSettings.))
; the factory that is used to create songs and other TG models. Overriden to have more readable objets.
(def ^TGFactory tg-factory 
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
    (let [^GTPInputStream gpis (new-GPInputStream format)]
      (. gpis (init tg-factory input-stream))
      (. gpis (readSong)))
    )
  )



(defn- percussion-track?
  "Whether a track is a percussion track"
  [^TGTrack track]
  ;we know it's a percussion track because the value of a string is 0. Very ugly, but that's how its programmed.
  (= 0 (.. track (getString 1) (getValue))))

; Represents a set of notes played on a instrument at a point in time, which have the same duration.
(defrecord NotesGroup [position 
                       duration 
                       pitches])

(defn- to-ratio-duration 
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

(defn- ^long pitch-of-fretted-note
  "Computes a pitch from a TGNote and the TGString on which it is played.
Note with bend or tremolo bar effects have a simplified treatment : the final pitch is retained.
"
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

(defn- notes-of-track
 "Transform a TGTrack into a sequence of NoteGroups." 
 [^TGTrack tg-track]                      
 (let [tgstring-with-number (fn [number] (.. tg-track (getString number))),
       measures (iterator-seq (.. tg-track (getMeasures))),
       find-notes-in-measure (fn [^TGMeasure measure 
                                  initial-offset]
                              (loop [remaining-beats (. measure getBeats),
                                     position initial-offset,
                                     notes-in-measure []]
                                ;looping through each beat in the measure
                                (if (empty? remaining-beats)
                                    {:final-offset position,
                                     :notes notes-in-measure}
                                    (let [^TGBeat current-beat (first remaining-beats),
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
               (long new-offset)
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
        

(def supported-formats #{:gp1 :gp2 :gp3 :gp4 :gp5})
(def format-to-extension {:gp1 "gtp",
                          :gp2 "gp2",
                          :gp3 "gp3",
                          :gp4 "gp4",
                          :gp5 "gp5"})
(def extension-to-format {"gtp" :gp1,
                          "gp2" :gp2,
                          "gp3" :gp3,
                          "gp4" :gp4,
                          "gp5" :gp5})

(defrecord FileAndFormat [^File file 
                          format])

(let [right-extension? (fn [^File file]
                         (supported-formats (extension-to-format (FilenameUtils/getExtension (. file getName)))))]
  (defn gp-files-seq 
    "Returns the files under the specified folder that have the right extension to be songs files, as a lazy seq"
    [root-dir]
    (filter right-extension? (file-seq (cio/file root-dir)))))


(defn- ^FileAndFormat to-FileAndFormat [^File file]
      (->FileAndFormat
        file
        (extension-to-format (FilenameUtils/getExtension (. file getName)))))

(defrecord TgSong&File [^TGSong tg-song 
                        ^File file])

(defn to-tg-song [^FileAndFormat f&f]
          (let [{file :file
                          fmt :format} f&f]
          (->TgSong&File (read-song (cio/input-stream file) fmt) 
                         file)))
(def problems-with-tg-songs (atom {}))
(def to-tg-song-safe (failing-to-nil to-tg-song
                                     (fn report-problem [[{file :file
                                                           fmt :format}]
                                                         exception]
                                       (swap! problems-with-tg-songs assoc file exception))))


(def all-gp-files-seq #(gp-files-seq all-songs-directory))
(def some-gp-files-seq #(take-sparsely 1000 (gp-files-seq development-songs-directory)))

(def all-tg-songs
  #(filter not-nil? (map (comp to-tg-song-safe to-FileAndFormat) (all-gp-files-seq))))
(def a-few-tg-songs
  #(filter not-nil? (map (comp to-tg-song-safe to-FileAndFormat) (some-gp-files-seq))))

(defrecord NiceSong [name
                     artist 
                     content])

(defn ^NiceSong to-nice-song
  [^TgSong&File song-and-file]
  (if (nil? song-and-file)
    nil            
    (let [{^TGSong tg-song :tg-song, ^File file :file} song-and-file
          name (. tg-song getName)
          artist (. file getParent)
          content (notes-of-song tg-song)]
      (->NiceSong name artist content))))

(def problems-with-nice-songs (atom {}))
(def to-nice-song-safe (failing-to-nil to-nice-song
                                       (fn report-problem [[{file :file}]
                                                           exception]
                                         (swap! problems-with-nice-songs assoc file exception))))
                                         

(def fetch-all-songs #(filter not-nil? (map (comp to-nice-song-safe to-tg-song-safe to-FileAndFormat) (all-gp-files-seq))))
(def fetch-a-few-songs #(filter not-nil? (map (comp to-nice-song-safe to-tg-song-safe to-FileAndFormat) (some-gp-files-seq))))

(defn ^NiceSong read-NiceSong-from-resource
  [file-name artist]
  (->NiceSong (FilenameUtils/getBaseName file-name)
              artist
              (notes-of-song (read-song (resource-as-stream file-name)
                                        (extension-to-format (FilenameUtils/getExtension file-name))))))
              
  
  

;some sample songs
(def chello (read-NiceSong-from-resource "sample-gp3.gp3" "Bach"))
(def rhapsody (read-NiceSong-from-resource "rhapsody.gp4" "Queen"))
(def au-clair-de-la-lune (read-NiceSong-from-resource "au_clair_de_la_lune.gp4" "Unknown"))
(def drums (read-NiceSong-from-resource "drums.gp4" "Val"))
(def several-instruments (read-NiceSong-from-resource "several_instruments.gp4" "Val"))
(def two-voices (read-NiceSong-from-resource "2_voices.gp4" "Val"))
(def smoke-water (read-NiceSong-from-resource "smoke_on_the_water.gp3" "Deep Purple"))
(def bend (read-NiceSong-from-resource "bend.gp4" "Val"))
(def all-samples [bend
                  chello 
                  rhapsody 
                  au-clair-de-la-lune 
                  drums 
                  several-instruments
                  two-voices
                  smoke-water])        
        

        
        
    
    
        
  
      


          
            
      
       
        
        
      
      
          





