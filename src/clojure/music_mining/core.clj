(ns music-mining.core
  (:import (java.io InputStream InputStreamReader BufferedReader)
           (org.herac.tuxguitar.io.gtp GP1InputStream 
                                       GP2InputStream
                                       GP3InputStream
                                       GP4InputStream
                                       GP5InputStream
                                       GTPSettings)
           (org.herac.tuxguitar.io.tg TGInputStream)
           (org.herac.tuxguitar.song.models TGSong)
           (org.herac.tuxguitar.song.factory TGFactory)
           )
  (:require [clojure.java.io :as cio]
            )
  (:use clojure.repl)
  )

; loads a resource on the classpath as an (opened!) stream.
(defn resource-as-stream [resource-name]
  (. (cio/resource resource-name) openStream))

; creating setting for GTP readers
(def settings (GTPSettings.))
; the factory that is used to create songs and other TG models
(def tg-factory (TGFactory.))

(let [GPInputStream-factory-method-for-format {:gp1 (fn [] (GP1InputStream. settings)),
                                               :gp2 (fn [] (GP2InputStream. settings)),
                                               :gp3 (fn [] (GP3InputStream. settings)),
                                               :gp4 (fn [] (GP4InputStream. settings)),
                                               :gp5 (fn [] (GP5InputStream. settings)),
                                               :tg (fn [] (TGInputStream.))}
      new-GPInputStream (fn [format] ((GPInputStream-factory-method-for-format format)))]
  (defn read-song
    "Reads a song of the specified format from the provided InputStream"
    [input-stream, format]
    (let [gpis (new-GPInputStream format)]
      (. gpis (init tg-factory input-stream))
      (. gpis (readSong)))
    ))


    

