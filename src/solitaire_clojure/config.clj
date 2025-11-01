(ns solitaire-clojure.config
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn- load-config [filename]
  (-> (io/resource filename) ; 1. Find the file in 'resources'
      (slurp)                ; 2. Read it into a single string
      (edn/read-string)))    ; 3. Parse the string into a Clojure map

;; This 'def' now creates the 'config' var by loading it from the file.
(def config (load-config "config.edn"))

