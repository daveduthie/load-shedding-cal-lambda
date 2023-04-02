(ns io.github.daveduthie.load-shedding-cal-lambda.scrape
  (:require [clojure.string :as str]
            [babashka.pods :as pods]
            [org.httpkit.client :as http])
  (:import (java.time LocalTime MonthDay Year ZoneId LocalDate)
           (java.time.format DateTimeFormatter)))

(pods/load-pod 'retrogradeorbit/bootleg "0.1.9")

(require '[pod.retrogradeorbit.bootleg.utils :as hickory.utils]
         '[pod.retrogradeorbit.hickory.select :as hickory.select])

(def ^:private ct-load-shedding-url
  "https://www.capetown.gov.za/Family%20and%20home/Residential-utility-services/Residential-electricity-services/Load-shedding-and-outages")

(defonce ^:private ct-load-shedding-html
  (:body @(http/get ct-load-shedding-url)))

(def ^:private load-shed-line-re
  #"Stage (\d)(?: \(no load-shedding\))?(: (underway until|\d{2}:\d{2}) (?:- )?(\d{2}:\d{2}))?")

(def ^:private date-pattern (DateTimeFormatter/ofPattern "d MMMM"))

(defn- parse-date
  [date-str]
  (-> (MonthDay/parse date-str date-pattern)
      (.atYear (.getValue (Year/now)))))

(defn try-parse-date
  [date-str]
  (try (parse-date date-str) (catch Exception e nil)))

(defn schedule-text
  []
  (let [the-div-contents
          (->> (hickory.utils/convert-to ct-load-shedding-html :hickory)
               (hickory.select/select (hickory.select/and
                                        (hickory.select/tag :div)
                                        (hickory.select/class "section-pull")
                                        (hickory.select/nth-child 1)))
               first
               :content)]
    (keep (fn [x]
            (cond (and (string? x) (not (str/blank? x))) (str/trim x)
                  (= (:tag x) :strong) (str/trim (first (:content x)))))
          the-div-contents)))

(comment
  (parse-date "2 April")
  (parse-date "02 April")
  (parse-date "20 April"))

(def ^:private jhb-zone (ZoneId/of "Africa/Johannesburg"))

;; TODO: find another "underway until" example and adapt
(defn- parse-schedule-text
  [line]
  ;; TODO: improve regex to drop _start-end group
  (when-let [[_ stage _start-end start end] (re-matches load-shed-line-re line)]
    {:stage stage, :start start, :end end, :raw/line line}))

(defn- end-time
  [end]
  (if (or (nil? end) ; omitted if whole day
          (= "24:00" end)) ; 24:00 doesn't parse
    "00:00" ; map to start of next day
    end))

(defn parse-times
  [date {:as interval, :keys [start end]}]
  (let [start-time (LocalTime/parse (or start "00:00"))
        end-time (LocalTime/parse (end-time end))]
    (-> interval
        (assoc :date date)
        (update :stage #(Integer/parseInt %))
        (assoc :start (-> date
                          (.atTime start-time)
                          (.atZone jhb-zone))
               :end (-> date
                        (cond-> (not (.isAfter end-time start-time)) (.plusDays
                                                                       1))
                        (.atTime end-time)
                        (.atZone jhb-zone))))))

(defn schedule*
  []
  (into []
        (comp (keep (some-fn parse-schedule-text try-parse-date))
              (partition-by (partial instance? LocalDate))
              (partition-all 2)
              (mapcat (fn [[[date] times]] (map #(parse-times date %) times))))
        (schedule-text)))

(defn schedule
  []
  (let [schedule_ (schedule*)
        extend-to-end-of-day (fn [intvl]
                               (update intvl
                                       :end
                                       (fn [zdt]
                                         (-> zdt
                                             (.withHour 23)
                                             (.withMinute 59)))))
        whole-day (fn [stage date]
                    {:stage stage,
                     :date date,
                     :start (-> date
                                (.atTime (LocalTime/of 0 0))
                                (.atZone jhb-zone)),
                     :end (-> date
                              (.atTime (LocalTime/of 23 59))
                              (.atZone jhb-zone)),
                     :guess true,
                     :raw/line "Synthetic! Extended last stage to more days"})
        {:keys [stage date]} (last schedule_)]
    (-> schedule_
        ;; TODO: maybe twitter is a more up-to-date source of info?
        (update (dec (count schedule_)) extend-to-end-of-day)
        (into [(whole-day stage (.plusDays date 1))
               (whole-day stage (.plusDays date 2))
               (whole-day stage (.plusDays date 3))]))))

(comment
  (schedule*)
  (schedule))

(comment
  (import '(java.time ZonedDateTime))
  (java.util.Date/from (.toInstant (ZonedDateTime/now)))
  (tap> (parse-times "21 March" (parse-schedule-text "Stage 1: 16:00 - 22:00")))
  :.)
