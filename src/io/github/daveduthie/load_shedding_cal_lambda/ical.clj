(ns io.github.daveduthie.load-shedding-cal-lambda.ical
  (:import (java.time ZonedDateTime)
           (java.time.format DateTimeFormatter)))

(def ^:private timestamp-formatter
  (DateTimeFormatter/ofPattern "';TZID='VV:yyyyMMdd'T'HHmmss"))

(defn event
  [start end title]
  (let [now (ZonedDateTime/now)]
    (doto (StringBuilder.)
      (.append "\nBEGIN:VEVENT")
      (.append (str "\nUID:" (java.util.UUID/randomUUID)))
      (.append (str "\nDTSTAMP" (.format now timestamp-formatter)))
      (.append (str "\nDTSTART" (.format start timestamp-formatter)))
      (.append (str "\nDTEND" (.format end timestamp-formatter)))
      (.append (str "\nSUMMARY:" title))
      (.append "\nEND:VEVENT"))))

(def ^:private preamble
  "BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//io.github.daveduthie//load-shedding-calendar//EN")

(defn ical
  [events]
  (let [sb (StringBuilder. preamble)]
    (run! #(.append sb %) events)
    (.append sb "\nEND:VCALENDAR")
    (.toString sb)))

(comment
  (tap> (ical [(event (ZonedDateTime/now)
                      (.plusMinutes (ZonedDateTime/now) 45)
                      "Do things")
               (event (ZonedDateTime/now)
                      (.plusMinutes (ZonedDateTime/now) 90)
                      "Do other things")])))
