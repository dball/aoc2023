#!/usr/bin/env bb

(require '[clojure.java.io :as io])
(require '[clojure.string :as string])

(defn parse-color [s]
  (let [[_ number-s color] (re-find #"^(\d+) (\w+)$" s)]
    [color (Integer/parseInt number-s)]))

(defn parse-sample [s]
  (into {} (map parse-color (string/split s #"\s*,\s*"))))

(defn parse-line [s]
  (let [[_ id samples-s] (re-find #"^Game (\d+): (.+)$" s)
        samples (string/split samples-s #"\s*;\s*")]
    [(Integer/parseInt id) (map parse-sample samples)]))

(def colors #{"red" "green" "blue"})

(defn least-population [game]
  (into {} (map (fn [color]
                  (let [counts (map (fn [sample] (get sample color 0)) game)]
                    [color (apply max counts)]))
                colors)))

(defn power [game]
  (apply * (vals (least-population game))))

(let [input (line-seq (io/reader *in*))
      games (into {} (map parse-line input))]
  (println (apply + (map power (vals games)))))