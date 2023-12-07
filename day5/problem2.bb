#!/usr/bin/env bb

(require '[clojure.java.io :as io])
(require '[clojure.string :as string])
(require '[clojure.edn :as edn])

(defn parse-seeds [header]
  (let [values (-> (first header)
                   (string/replace #"^seeds:" "[")
                   (string/replace #"$" "]")
                   edn/read-string)
        chunks (partition-all 2 values)]
    (into (sorted-map) (map (fn [[start length]] [start {:length length}]) chunks))))

(defn parse-mapping [mapping]
  (let [[dest-start source-start length] (edn/read-string (str "[" mapping "]"))]
    [source-start
     {:delta (- dest-start source-start)
      :length length}]))

(defn parse-mappings [mappings]
  (into (sorted-map) (map parse-mapping (rest mappings))))

(defn eval-domain [mappings domain]
  (let [[start {:keys [length]}] domain]
    (loop [codomain (sorted-map)
           left (rsubseq mappings < start)
           right (subseq mappings >= start)
           start start
           length length]
      (let [end (+ start length)
            [[left-start {left-length :length left-delta :delta}]] left
            [[right-start {right-length :length right-delta :delta}]] right]
        (cond
          (= 0 length)
          codomain

          (seq left)
          (let [left-end (+ left-start left-length)]
            (cond
              (<= left-end start)
              (recur codomain nil right start length)

              (<= end left-end)
              (assoc codomain (+ start left-delta) {:length length})

              :else
              (let [overlap (- left-end start)
                    codomain' (assoc codomain (+ start left-delta) {:length overlap})]
                (recur codomain' nil right left-end (- length overlap)))))

          (or (not (seq right))
              (<= end right-start))
          (assoc codomain start {:length length})

          (< start right-start)
          (let [gap (- right-start start)
                codomain' (assoc codomain start {:length gap})]
            (recur codomain' nil right right-start (- length gap)))

          :else
          (let [right-end (+ right-start right-length)]
            (cond
              (<= end right-end)
              (assoc codomain (+ start right-delta) {:length length})

              :else
              (let [codomain' (assoc codomain (+ start right-delta) {:length right-length})]
                (recur codomain' nil (rest right) right-end (- length right-length))))))))))

(defn eval-domains [mappings domains]
  (into (sorted-map) (map (partial eval-domain mappings) domains)))

(let [input (line-seq (io/reader *in*))
      [header & mappings] (remove (partial = [""]) (partition-by (fn [line] (= line "")) input))
      seeds (parse-seeds header)
      all-mappings (mapv parse-mappings mappings)]
  (prn (ffirst (loop [domains seeds
                      remaining-mappings all-mappings]
                 (if-not (seq remaining-mappings)
                   domains
                   (let [codomains (eval-domains (first remaining-mappings) domains)]
                     (recur codomains (rest remaining-mappings))))))))