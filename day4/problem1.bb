#!/usr/bin/env bb

(require '[clojure.java.io :as io])
(require '[clojure.string :as string])
(require '[clojure.edn :as edn])
(require '[clojure.set :as set])

(defn parse-line [s]
  (-> s
      (string/replace #"^Card" "[")
      (string/replace #"$" "}]")
      (string/replace #"\:" " #{")
      (string/replace #"\|" "} #{") 
      (edn/read-string)))

(defn score [card]
  (let [[_ winners haves] card
        have-wins (count (set/intersection winners haves))]
    (if (zero? have-wins)
      0
      (int (Math/pow 2 (dec have-wins))))))

(defn eval-card [card]
  (let [[id winners haves] card
        wins (count (set/intersection winners haves))]
    [id {:wins wins :count 1}]))

(defn eval-cards [cards]
  (reduce (fn [accum card-id]
            (let [card (accum card-id)]
              (loop [accum accum
                     offset 0]
                (if (= offset (:wins card))
                  accum
                  (recur (update-in accum [(+ card-id offset 1) :count] + (:count card)) (inc offset))))))
          (into {} (map eval-card cards))
          (map first cards)))

(let [input (line-seq (io/reader *in*))
      lines (map parse-line input)] 
  (println (apply + (map score lines)))
  (println (apply + (map :count (vals (eval-cards lines))))))