(ns io.github.daveduthie.load-shedding-cal-lambda.core
  (:require [fierycod.holy-lambda-ring-adapter.core :as hlra]
            [fierycod.holy-lambda.agent :as agent]
            [fierycod.holy-lambda.core :as h]
            [io.github.daveduthie.load-shedding-cal-lambda.ical :as ical]
            [io.github.daveduthie.load-shedding-cal-lambda.timetable :as
             timetable]
            [io.github.daveduthie.load-shedding-cal-lambda.scrape :as scrape])
  (:gen-class))

(defn valid-interval? [{:keys [start end]}] (.isBefore start end))

(defn intersection
  [{s1 :start, e1 :end} {s2 :start, e2 :end}]
  (let [later-start (if (.isAfter s1 s2) s1 s2)
        earlier-end (if (.isBefore e1 e2) e1 e2)
        intvl {:start later-start, :end earlier-end}]
    (when (valid-interval? intvl) intvl)))

(defn load-shedding
  [zone]
  (let [schedule (scrape/schedule)]
    (mapcat (fn [{:keys [stage start guess], :as intvl}]
              (let [schedule-for-date (timetable/timetable-for-stage-and-zone
                                        stage
                                        zone
                                        (.toLocalDate start))]
                (into []
                      (comp (keep (partial intersection intvl))
                            (map #(assoc %
                                    :stage stage
                                    :guess guess)))
                      schedule-for-date)))
      schedule)))

(defn- event-title
  [guess stage]
  (str (format "%sLoad Shedding (Stage %s)" (if guess "[?] " "") stage)))

(defn app
  [request]
  (if-let [zone-id (try (some-> request
                                :lambda
                                :event :queryStringParameters
                                :zone_id Integer/parseInt)
                        (catch Exception _e nil))]
    {:status 200,
     :headers {"Content-Type" "text/calendar"},
     :body (ical/ical (map (fn [{:keys [start end stage guess]}]
                             (ical/event start end (event-title guess stage)))
                        (load-shedding zone-id)))}
    {:status 400,
     :headers {"Content-Type" "application/json"},
     :body "{\"message\": \"Missing or malformed zone_id\"}"}))

(def Lambda (hlra/ring<->hl-middleware app))
(h/entrypoint [#'Lambda])

;; Executes the body in a safe agent context for native configuration
;; generation.
;; Useful when it's hard for agent payloads to cover all logic branches.
(agent/in-context (println
                    "I will help in generation of native-configurations"))

(comment
  (spit "x.ics"
        (ical/ical (map (fn [{:keys [start end stage]}]
                          (ical/event start
                                      end
                                      (str (format "Load Shedding (Stage %s)"
                                                   stage))))
                     (load-shedding 2)))))
