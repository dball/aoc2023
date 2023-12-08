#!/usr/bin/env bb

(require '[clojure.math :as math])

(defn winning-times [time record-distance]
  (let [radical (math/sqrt (- (* time time) (* 4 record-distance)))
        lower (/ (- time radical) 2)
        lower-round (math/ceil lower)
        lower-int (cond-> (long lower-round) (= lower lower-round) inc)
        upper (/ (+ time radical) 2)
        upper-round (math/floor upper)
        upper-int (cond-> (long upper-round) (= upper upper-round) dec)]
    (prn time record-distance lower lower-round lower-int upper upper-round upper-int)
    (range lower-int (inc upper-int))))

(prn (reduce * (map count [(winning-times 7 9) (winning-times 15 40) (winning-times 30 200)])))

(prn (reduce * (map count [(winning-times 58 434) (winning-times 81 1041) (winning-times 96 2219) (winning-times 76 1218)])))

(prn (count (winning-times 71530 940200)))

(prn (count (winning-times 58819676 434104122191218)))