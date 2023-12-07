#!/usr/bin/env bb

(require '[clojure.java.io :as io])
(require '[clojure.string :as string])

(def numbers 
  {"one" 1
   "two" 2
   "three" 3
   "four" 4
   "five" 5
   "six" 6
   "seven" 7
   "eight" 8
   "nine" 9
   "1" 1
   "2" 2
   "3" 3
   "4" 4
   "5" 5
   "6" 6
   "7" 7
   "8" 8
   "9" 9})

(def rev-numbers (into {} (map (fn [[s n]] [(string/reverse s) n]) numbers)))

(defn build-number-re
  [numbers]
  (re-pattern (string/join "|" (sort (keys numbers)))))

(def number-re (build-number-re numbers))

(def rev-number-re (build-number-re rev-numbers))

(defn line-number [s]
  (let [first-number (numbers (first (re-seq number-re s)))
        last-number (rev-numbers (first (re-seq rev-number-re (string/reverse s))))]
    (+ (* 10 first-number) last-number)))

(let [input (line-seq (io/reader *in*))
      all-numbers (map line-number input)] 
  (println (apply + all-numbers)))