#!/usr/bin/env bb

(require '[clojure.java.io :as io])
(require '[clojure.string :as string])

(def card-values
  {\A 14 \K 13 \Q 12 \J 11 \T 10 \9 9 \8 8 \7 7 \6 6 \5 5 \4 4 \3 3 \2 2 \1 1})

(defn hand-type [hand]
  (case (sort (vals (frequencies hand)))
    [5] 7
    [1 4] 6
    [2 3] 5
    [1 1 3] 4
    [1 2 2] 3
    [1 1 1 2] 2
    1))

(defn hand-value [hand]
  (into [(hand-type hand)] (map card-values hand)))

(let [input (line-seq (io/reader *in*))
      entries (map (fn [s] (let [[hand bid] (string/split s #" ")]
                             {:hand hand :bid (Integer/parseInt bid) :value (hand-value hand)})) input)
      ranked (map-indexed (fn [i entry] (assoc entry :rank (inc i) :winnings (* (inc i) (:bid entry)))) (sort-by :value entries))
      winnings (reduce + 0 (map :winnings ranked))]
  (prn winnings))
