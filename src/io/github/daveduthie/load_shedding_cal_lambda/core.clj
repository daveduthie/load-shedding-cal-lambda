(ns io.github.daveduthie.load-shedding-cal-lambda.core
  (:require
    [fierycod.holy-lambda-ring-adapter.core :as hlra]
    [fierycod.holy-lambda.agent :as agent]
    [fierycod.holy-lambda.core :as h]
    [io.github.daveduthie.load-shedding-cal-lambda.ical :as ical]
    [io.github.daveduthie.load-shedding-cal-lambda.interval :as interval]
    [io.github.daveduthie.load-shedding-cal-lambda.scrape :as scrape]
    [io.github.daveduthie.load-shedding-cal-lambda.timetable :as timetable])
  (:gen-class))

(defn load-shedding-for-zone
  [zone]
  (let [schedule (scrape/schedule)]
    (mapcat (fn [{:keys [stage start guess], :as intvl}]
              (let [timetable (timetable/timetable-for-stage-and-zone
                                stage
                                zone
                                (.toLocalDate start))]
                (into []
                      (comp (keep (partial interval/intersection intvl))
                            (map #(assoc %
                                    :stage stage
                                    :guess guess)))
                      timetable)))
      schedule)))

(defn- get-ical
  [zone-id]
  (ical/ical (map (fn [{:keys [start end stage guess]}]
                    (ical/event start
                                (.plusMinutes end 30)
                                (format "%sLoad Shedding (Stage %s)"
                                        (if guess "[?] " "")
                                        stage)))
               (load-shedding-for-zone zone-id))))

(defn app
  [request]
  (if-let [zone-id (try (some-> request
                                :lambda
                                :event :queryStringParameters
                                :zone_id Integer/parseInt)
                        (catch Exception _e nil))]
    {:status 200,
     :headers {"Content-Type" "text/calendar"},
     :body (get-ical zone-id)}
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
                     (load-shedding-for-zone 2)))))
