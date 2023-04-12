(ns io.github.daveduthie.load-shedding-cal-lambda.interval)

(defn valid-interval? [{:keys [start end]}] (.isBefore start end))

(defn intersection
  [{s1 :start, e1 :end} {s2 :start, e2 :end}]
  (let [later-start (if (.isAfter s1 s2) s1 s2)
        earlier-end (if (.isBefore e1 e2) e1 e2)
        intvl {:start later-start, :end earlier-end}]
    (when (valid-interval? intvl) intvl)))

