(ns io.github.daveduthie.load-shedding-cal-lambda.scrape
  (:require [clojure.string :as str]
            [io.github.daveduthie.load-shedding-cal-lambda.common :as common]
            [org.httpkit.client :as http])
  (:import (java.time LocalDate LocalTime MonthDay Year)
           (java.time.format DateTimeFormatter)))

(def ^:private ct-load-shedding-url
  "https://www.capetown.gov.za/Family%20and%20home/Residential-utility-services/Residential-electricity-services/Load-shedding-and-outages")

(defonce ^:private ct-load-shedding-html
  (:body @#_{:clj-kondo/ignore [:unresolved-var]}
          (http/get ct-load-shedding-url)))

(def ^:private date-pattern (DateTimeFormatter/ofPattern "d MMMM"))

(defn- parse-date
  [date-str]
  ;; FIXME: wrong around new year...
  (-> (MonthDay/parse date-str date-pattern)
      (.atYear (.getValue (Year/now)))))

(defn try-parse-date
  [date-str]
  (try (parse-date date-str) (catch Exception _e nil)))

(comment
  (parse-date "2 April")
  (parse-date "02 April")
  (parse-date "20 April"))

(def ^:private load-shed-line-re
  #"Stage (\d)(?: \(no load-shedding\))?: (underway until|\d{2}:\d{2}) (?:- )?(\d{2}:\d{2})")

(def ^:private load-shed-line-re2 #"Stage (\d) until further notice.")

;; TODO: find another "underway until" example and adapt
(defn- parse-schedule-text
  [line]
  (if-let [[_ stage start end] (re-matches load-shed-line-re line)]
    {:stage stage, :start start, :end end, :raw/line line}
    (when-let [[_ stage] (re-matches load-shed-line-re2 line)]
      {:stage stage, :start "00:00", :end "00:00", :raw/line line})))

(defn- end-time
  [end]
  (if (or (nil? end)       ; omitted if whole day
          (= "24:00" end)) ; 24:00 doesn't parse
    "00:00"                ; map to start of next day
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
                          (.atZone common/zone))
               :end (-> date
                        (cond-> (not (.isAfter end-time start-time)) (.plusDays
                                                                       1))
                        (.atTime end-time)
                        (.atZone common/zone))))))

(def schedule-lines
  (->> (-> (re-find #"City customers:((?s).*)Subject to rapid change."
                    ct-load-shedding-html)
           second
           (str/replace #"</?[a-z0-9]+>" ""))
       str/split-lines
       (remove str/blank?)
       (map str/trim)))

(def schedule*
  (into []
        (comp (keep (some-fn parse-schedule-text try-parse-date))
              (partition-by (partial instance? LocalDate))
              (partition-all 2)
              (mapcat (fn [[[date] times]] (map #(parse-times date %) times))))
        schedule-lines))

(defn schedule
  []
  (let [schedule_ schedule*
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
                                (.atZone common/zone)),
                     :end (-> date
                              (.atTime (LocalTime/of 0 0))
                              (.atZone common/zone)
                              (.plusDays 1)),
                     :guess true,
                     :raw/line "Synthetic! Extended last stage to more days"})
        {:keys [stage date]} (last schedule_)]
    (-> schedule_
        (update (dec (count schedule_)) extend-to-end-of-day)
        (into [(whole-day stage (.plusDays date 1))
               (whole-day stage (.plusDays date 2))
               (whole-day stage (.plusDays date 3))]))))

(comment
  schedule*
  (schedule))
