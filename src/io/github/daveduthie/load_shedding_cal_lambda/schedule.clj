(ns io.github.daveduthie.load-shedding-cal-lambda.schedule
  (:require [io.github.daveduthie.load-shedding-cal-lambda.common :as common])
  (:import (java.time LocalDate LocalTime)))

(defn- column [day-of-month] (mod (dec day-of-month) 16))

(defn- offset [day-of-month] (* 12 (column day-of-month)))

(comment
  (offset 18))

(defn- shift [day-of-month] (quot (column day-of-month) 4))

(defn- stage->initial-zones [stage] (vec (take stage [0 8 12 4 1 9 13 5])))

(defn schedule
  [stage day-of-month]
  (when-not (zero? stage)
    (let [zone-seq (drop (+ (shift day-of-month) (offset day-of-month))
                         (cycle (range 1 17)))
          zone-seqs (map (fn [zone-id] (take 12 (drop zone-id zone-seq)))
                      (stage->initial-zones stage))]
      (apply (partial map (comp vec sort vector)) zone-seqs))))

(comment
  (vec (schedule 8 2))
  :.)

(def ^:private hours (map (fn [hour] (LocalTime/of hour 0)) (range 0 24 2)))

(defn load-shedding-for-zone
  [stage date zone]
  (filter #(-> %
               :zones
               (contains? zone))
    (map (fn [hour zones]
           (let [start (-> date
                           (.atTime hour)
                           (.atZone common/zone))]
             {:start start,
              :end (-> start
                       (.plusHours 2)
                       (.plusMinutes 30)),
              :stage stage,
              :zones zones}))
      hours
      (schedule stage (.getDayOfMonth date)))))

(comment
  (load-shedding-for-zone 3 (LocalDate/now) 2)
  (schedule 2 25)
  (tap> {[2 25] (schedule 2 25),
         [5 24] (schedule 5 24),
         [3 31] (schedule 3 31),
         [7 17] (schedule 7 17)})
  (column 28))

