(ns request-server-lib.core
  (:require [clojure.string :as cstring]
            [utils-lib.core :as utils]
            [ajax-lib.http.mime-type :as mt]
            [ajax-lib.http.entity-header :as eh]
            [ajax-lib.http.request-header :as rh]
            [ajax-lib.http.general-header :as gh])
  (:import [java.net Socket]))

(defn- pack-request-from-map
  ""
  [request-map]
  (let [request (atom "")
        request-body (atom nil)
        request-start-line (:request-start-line request-map)
        headers (:headers request-map)]
    (swap!
      request
      str
      request-start-line)
    (doseq [[m-key
             m-val] headers]
      (swap!
        request
        str
        m-key
        ": "
        m-val
        "\r\n"))
    (when-let [body (:body request-map)]
      (let [body (if (= (get headers (eh/content-type))
                        (mt/text-plain))
                   (.getBytes
                     body
                     "UTF-8")
                   body)]
        (swap!
          request
          str
          (eh/content-length)
          ": "
          (count
            body)
          "\r\n\r\n")
        (reset!
          request-body
          body))
     )
    [@request
     @request-body]))

(defn pack-request
  ""
  [host
   port
   request-method
   uri
   body]
  (let [body (str
               body)
        body-bytes (.getBytes
                     body
                     "UTF-8")
        request-start-line (str
                             request-method
                             " "
                             uri
                             " HTTP/1.1\r\n")
        host-header (str
                      host
                      ":"
                      port)
        referer (str
                  "http://"
                  host
                  ":"
                  port
                  "/")
        request-map {:request-start-line request-start-line
                     :headers {(rh/host) host-header
                               (gh/connection) "keep-alive"
                               (rh/user-agent)
                                 "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:61.0) Gecko/20100101 Firefox/61.0"
                               (rh/accept) (mt/text-plain)
                               (rh/accept-language) "en-GB,en;q=0.5"
                               (rh/accept-encoding) "gzip, deflate, br"
                               (rh/referer) referer
                               (eh/content-type) (mt/text-plain)
                               (rh/cookie) "session=0149a841-1abd-4470-968c-c558d18cc8be"}
                     :body body-bytes}
        request (pack-request-from-map
                  request-map)]
    ;(print header)
    ;(print body)
    request))

(defn send-request
  ""
  [output-stream
   request-header
   request-body]
  (.write
    output-stream
    request-header)
  (.write
    output-stream
    request-body)
  (.flush
    output-stream))

(defn- read-header
  "Read header of XMLHTTPResponse"
  [header]
  (let [header-vector (cstring/split
                        header
                        #"\r\n")
        response-start-line (get header-vector 0)
        response-start-line-vector (cstring/split
                                     response-start-line
                                     #" "
                                     3)
        response-protocol (get response-start-line-vector 0)
        response-status-code (get response-start-line-vector 1)
        response-status (get response-start-line-vector 2)
        header-vector (utils/remove-index-from-vector
                        header-vector
                        0)
        header-map (atom
                     {:response-status response-status
                      :response-status-code response-status-code
                      :response-protocol response-protocol})]
    (doseq [header-line header-vector]
      (let [header-line-vector (cstring/split header-line #": " 2)
            key-name (cstring/lower-case (get header-line-vector 0))
            key-value (get header-line-vector 1)]
        (swap!
          header-map
          assoc
          (keyword
            key-name)
          key-value))
     )
    @header-map))

(defn process-response
  ""
  [input-stream]
  (let [body-separator [13 10 13 10]
        last-four-bytes (atom [0 0 0 0])
        header (atom [])
        read-stream (while (not= body-separator
                                 @last-four-bytes)
                      (let [read-byte (unchecked-byte
                                        (.read
                                          input-stream))]
                        (swap!
                          header
                          conj
                          read-byte)
                        (swap!
                          last-four-bytes
                          utils/remove-index-from-vector
                          0)
                        (swap!
                          last-four-bytes
                          conj
                          read-byte))
                     )
        remove-last-r-n (swap!
                          header
                          utils/remove-index-from-vector
                          (into
                            []
                            (range
                              (- (count @header)
                                 4)
                              (count @header))
                           ))
        header-string (String.
                        (byte-array
                          @header)
                        "UTF-8")
        header-map (read-header
                     header-string)
        content-length (when-let [content-length (:content-length header-map)]
                         (read-string content-length))
        body-bytes (atom [])
        read-stream (doseq [itr (range content-length)]
                      (let [read-byte (unchecked-byte
                                        (.read
                                          input-stream))]
                        (swap!
                          body-bytes
                          conj
                          read-byte))
                     )
        response (if-not (empty? @body-bytes)
                   (let [body (if (cstring/index-of
                                    (:content-type header-map)
                                    "text")
                                (String.
                                  (byte-array
                                    @body-bytes)
                                  "UTF-8")
                                @body-bytes)]
                     (assoc
                       header-map
                       :body
                       body))
                   header-map)]
    (println "\n\n"response)
    (when (not
            (contains?
              response
              :set-cookie))
      (println "failed"))
    ))

(defn fire-request
  ""
  [host
   port
   request-method
   uri
   body]
  (let [client-socket (Socket.
                        host
                        port)
        input-stream (.getInputStream
                       client-socket)
        output-stream (.getOutputStream
                        client-socket)
        [request-header
         request-body] (pack-request
                         host
                         port
                         request-method
                         uri
                         body)]
    (send-request
      output-stream
      request-header
      request-body)
    (process-response
      input-stream)
    (.close
      client-socket))
 )

#_(fire-request
  "sample"
  1616
  "POST"
  "/get-entities"
  {:entity-type "person"
   :entity-filter {}
   :qsort {:first-name 1}
   :collation {:locale "sr"}
   :rows 25
   :pagination true
   :projection-include true
   :current-page 0
   :projection [:first-name :last-name :height :weight :birthday :gender]})

#_(fire-request
  "sample"
  8449
  "POST"
  "/clojure/get-entities"
  {:entity-type "person"
   :entity-filter {}
   :qsort {:first-name 1}
   :collation {:locale "sr"}
   :rows 25
   :pagination true
   :projection-include true
   :current-page 0
   :projection [:first-name :last-name :height :weight :birthday :gender]})

#_(let [parallel-calls 10
      doseq-times 400
      req-count (* parallel-calls
                   doseq-times)
      result (map
               pcalls
               (repeat
                 parallel-calls
                 (fn []
                   (let [start-time (java.util.Date.)]
                     (doseq [i (range doseq-times)] (fire-request))
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

