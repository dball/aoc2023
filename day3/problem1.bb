#!/usr/bin/env bb

(require '[clojure.java.io :as io])

(defn add-part [engine id xs y]
  (let [points (into {} (map (fn [x] [[x y] {:type :part :id id :xs xs :y y}]) xs))]
    (-> engine
        (update :parts conj #{:id id :points points})
        (update :grid into points))))

(defn add-mount [engine sym x y]
  (-> engine
      (update :mounts conj {:sym sym :point [x y]})
      (update :grid assoc [x y] {:type :mount :sym sym})))

(defn add-line [engine s]
  (let [y (:next-y engine)]
    (loop [engine engine
           chunks (re-seq #"\d+|\.+|[^\d\.]" s)
           x 0]
      (if-not (seq chunks)
        engine
        (let [[chunk & chunks'] chunks
              x' (+ x (count chunk))
              char (first chunk)] 
          (cond
            (= \. char)
            (recur engine chunks' x')
            
            (Character/isDigit char)
            (let [id (Integer/parseInt chunk)
                  xs (range x (+ x (count chunk)))
                  engine (add-part engine id xs y)]
              (recur engine chunks' x'))
            
            :else
            (let [engine (add-mount engine char x y)]
              (recur engine chunks' x'))))))))

(defn neighbors [[x y]]
  (let [fs [dec identity inc]]
    (disj (into #{} (for [fx fs fy fs] [(fx x) (fy y)])) [x y])))

(defn nearby-parts [engine point]
  (reduce (fn [parts point]
            (let [value (get-in engine [:grid point])]
              (cond-> parts
                (= :part (:type value))
                (conj value))))
          #{}
          (neighbors point)))

(defn mounted-parts [engine]
  (reduce (fn [parts point]
            (into parts (nearby-parts engine point)))
          #{}
          (map :point (:mounts engine))))

(defn gear-ratios [engine]
  (keep (fn [mount]
            (when (= \* (:sym mount))
              (let [gear-parts (nearby-parts engine (:point mount))]
                (when (= 2 (count gear-parts))
                  (apply * (map :id gear-parts))))))
          (:mounts engine)))

(let [input (line-seq (io/reader *in*))
      base-engine {:parts #{} :mounts #{} :grid {} :next-y 0}
      engine (reduce (fn [engine line]
                       (update (add-line engine line) :next-y inc))
                     base-engine input)] 
  (println (apply + (map :id (mounted-parts engine))))
  (println (apply + (gear-ratios engine))))