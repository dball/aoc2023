#!/usr/bin/env bb

(require '[clojure.java.io :as io])
(require '[clojure.string :as string])
(require '[clojure.edn :as edn])

(defn parse-seeds [header]
  (-> (first header)
      (string/replace #"^seeds:" "#{")
      (string/replace #"$" "}")
      edn/read-string))

(defn build-mapping [mapping]
  (let [[dest-start source-start length] (edn/read-string (str "[" mapping "]"))]
    [source-start [dest-start length]]))

(defn parse-map [map]
  (let [[header & mappings] map
        [_ source dest] (re-find #"^(\w+)-to-(\w+) map:$" header)]
    {:source source
     :dest dest
     :mapping (reduce (fn [accum mapping] (conj accum (build-mapping mapping))) (sorted-map) mappings)}))

(defn resolve-value [mapping value]
  (let [entry (first (rsubseq mapping <= value))]
    (if (nil? entry)
      value
      (let [[source-start [dest-start length]] entry]
        (if (<= source-start value (+ source-start length))
          (+ dest-start (- value source-start))
          value)))))

(defn find-location [almanac seed]
  (loop [value seed
         maps (:maps almanac)]
    (if-not (seq maps)
      value
      (let [[map & maps'] maps
            value' (resolve-value (:mapping map) value)]
        (recur value' maps')))))

(let [input (line-seq (io/reader *in*))
      [header & maps] (remove (partial = [""]) (partition-by (fn [line] (= line "")) input))
      seeds (parse-seeds header)
      maps (mapv parse-map maps)
      almanac {:seeds seeds :maps maps}
      locations (into (sorted-set) (map (partial find-location almanac) seeds))]
  (prn (first locations)))