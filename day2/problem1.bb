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

(defn possible-sample? [population sample]
  (every? (fn [[color total]]
            (let [sampled-color (get sample color)]
              (or (nil? sampled-color) (>= total sampled-color))))
          population))

(defn possible-game? [population game]
  (every? (partial possible-sample? population) game))

(let [input (line-seq (io/reader *in*))
      games (into {} (map parse-line input))
      population {"red" 12 "green" 13 "blue" 14}
      possibilities (into {} (filter (fn [[_ game]] (possible-game? population game)) games))]
  (println (apply + (keys possibilities))))