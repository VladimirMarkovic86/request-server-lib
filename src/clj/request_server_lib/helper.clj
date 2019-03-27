(ns request-server-lib.helper)

(defn parallel-stress-requests
  "Stress server with parallel requests"
  []
  (let [parallel-calls 10
        doseq-times 400
        req-count (* parallel-calls
                     doseq-times)
        result (map
                 pcalls
                 (repeat
                   parallel-calls
                   (fn []
                     (let [start-time (java.util.Date.)]
                       (doseq [i (range doseq-times)]
                         (fire-request
                           "sample"
                           1603
                           "POST"
                           "/get-entities"
                           {:keystore-file-path
                             "./resources/certificate/sample_server.jks"
                            :keystore-password "ultras12"
                            :keystore-type nil
                            :ssl-context nil}
                           {:entity-type "person"
                            :entity-filter {}
                            :qsort {:first-name 1}
                            :collation {:locale "sr"}
                            :rows 25
                            :pagination true
                            :projection-include true
                            :current-page 0
                            :projection [:first-name
                                         :last-name
                                         :height
                                         :weight
                                         :birthday
                                         :gender]}))
                       (let [end-time (java.util.Date.)]
                         (- (.getTime
                              end-time)
                            (.getTime
                              start-time))
                        ))
                    ))
                )
        final-res (atom 0)]
    (doseq [res result]
      (swap!
        final-res
        +
        (first
          res))
     )
    (str
      (int
        (/ req-count
           (/ @final-res
              1000))
       )
      " RPS"))
 )

