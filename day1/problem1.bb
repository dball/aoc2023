#!/usr/bin/env bb

(require '[clojure.java.io :as io])

(def numbers (zipmap "0123456789" (range)))

(defn first-number [ss]
  (first (keep numbers ss)))

(let [input (line-seq (io/reader *in*))
      first-numbers (map (comp first-number seq) input)
      last-numbers (map (comp first-number reverse seq) input)]
  (println (+ (* 10 (apply + first-numbers)) (apply + last-numbers))))