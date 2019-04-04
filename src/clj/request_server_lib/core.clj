(ns request-server-lib.core
  (:require [clojure.string :as cstring]
            [utils-lib.core :as utils]
            [ajax-lib.http.mime-type :as mt]
            [ajax-lib.http.entity-header :as eh]
            [ajax-lib.http.request-header :as rh]
            [ajax-lib.http.general-header :as gh])
  (:import [java.net Socket]
           [javax.net.ssl SSLSocketFactory
                          TrustManagerFactory
                          SSLContext]
           [java.security KeyStore]
           [java.io FileInputStream]))

(defn pack-request-from-map
  "Packs request from clojure map into raw http request string with request body"
  [request-map]
  (let [request-headers (atom nil)
        request-start-line (:request-start-line request-map)
        headers (:headers request-map)]
    (when (and request-start-line
               (string?
                 request-start-line)
               (not
                 (cstring/blank?
                   request-start-line))
               headers
               (or (and (string?
                          headers)
                        (not
                          (cstring/blank?
                            headers))
                    )
                   (and (map?
                          headers)
                        (not
                          (empty?
                            headers))
                    ))
           )
      (swap!
        request-headers
        str
        request-start-line
        "\r\n")
      (when (and (string?
                   headers)
                 (not
                   (cstring/blank?
                     headers))
             )
        (swap!
          request-headers
          str
          headers))
      (when (and (map?
                   headers)
                 (not
                   (empty?
                     headers))
             )
        (doseq [[m-key
                 m-val] headers]
          (swap!
            request-headers
            str
            m-key
            ": "
            m-val
            "\r\n"))
       ))
    @request-headers))

(defn pack-request
  "Pack request with host, port, request method, uri and body"
  [host
   port
   request-method
   uri
   body]
  (when (and host
             (string?
               host)
             (not
               (cstring/blank?
                 host))
             port
             (number?
               port)
             request-method
             (string?
               request-method)
             (not
               (cstring/blank?
                 request-method))
             uri
             (string?
               uri)
             (not
               (cstring/blank?
                 uri))
         )
    (let [request-start-line (str
                               request-method
                               " "
                               uri
                               " HTTP/1.1")
          accept-value "*/*"
          accept-encoding-value "gzip, deflate, br"
          accept-language-value "sr,en;q=0.5"
          no-cache-value "no-cache"
          connection-value "keep-alive"
          host-header (if (= port
                             443)
                        host
                        (str
                          host
                          ":"
                          port))
          origin (if (or (= port
                            443)
                         (= port
                            80))
                   (if (= port
                            80)
                     (str
                       "http://"
                       host)
                     (str
                       "https://"
                       host))
                   (str
                     "https://"
                     host
                     ":"
                     port))
          referer (if (or (= port
                             443)
                          (= port
                             80))
                    (if (= port
                             80)
                      (str
                        "http://"
                        host
                        "/")
                      (str
                        "https://"
                        host
                        "/"))
                    (str
                      "https://"
                      host
                      ":"
                      port
                      "/"))
          user-agent-value "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:61.0) Gecko/20100101 Firefox/61.0"
          request-body-a (atom nil)
          request-map {:request-start-line request-start-line
                       :headers (sorted-map
                                  (rh/accept) accept-value
                                  (rh/accept-encoding) accept-encoding-value
                                  (rh/accept-language) accept-language-value
                                  (gh/cache-control)	no-cache-value
                                  (gh/connection) connection-value
                                  (rh/host) host-header
                                  (rh/origin) origin
                                  (gh/pragma) no-cache-value
                                  (rh/referer) referer
                                  (rh/user-agent) user-agent-value)}
          raw-request-header (pack-request-from-map
                               request-map)
          raw-request-header-a (atom
                                 raw-request-header)]
      (if-let [body body]
        (let [content-type-value (atom nil)]
          (when (or (map?
                      body)
                    (vector?
                      body)
                    (set?
                      body)
                    (seq?
                      body))
            (reset!
              content-type-value
              (mt/text-clojurescript))
           )
          (when (and (string?
                       body)
                     (not
                       (cstring/blank?
                         body))
                 )
            (reset!
              content-type-value
              (mt/text-plain))
           )
          (when @content-type-value
            (swap!
              raw-request-header-a
              str
              (eh/content-type)
              ": "
              @content-type-value
              "\r\n"))
          (let [body (if (and (contains?
                                #{(mt/text-plain)
                                  (mt/text-clojurescript)}
                                @content-type-value)
                              (not
                                (bytes?
                                  body))
                          )
                       (.getBytes
                         (if (= @content-type-value
                                (mt/text-clojurescript))
                           (str
                             body)
                           body)
                         "UTF-8")
                       body)]
            (swap!
              raw-request-header-a
              str
              (eh/content-length)
              ": "
              (count
                body)
              "\r\n\r\n")
            (reset!
              request-body-a
              body))
         )
        (swap!
          raw-request-header-a
          str
          "\r\n"))
      [@raw-request-header-a
       @request-body-a]))
 )

(defn send-request
  "Sends request to host defined in request header"
  [output-stream
   request-header
   request-body]
  (try
    (when (and output-stream
               request-header
               (bytes?
                 request-header))
      (.write
        output-stream
        request-header)
      (when (and request-body
                 (bytes?
                   request-body))
        (.write
          output-stream
          request-body))
      (.flush
        output-stream)
      true)
    (catch Exception e
      (println
        (.getMessage
          e))
     ))
 )

(defn read-header
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
                     {:status response-status
                      :status-code response-status-code
                      :protocol response-protocol})]
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
  "Read response from input stream"
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
                         (read-string
                           content-length))
        body-bytes (atom [])
        read-stream (dotimes [i content-length]
                      (let [read-byte (unchecked-byte
                                        (.read
                                          input-stream))]
                        (swap!
                          body-bytes
                          conj
                          read-byte))
                     )
        response (if-not (empty?
                           @body-bytes)
                   (let [response-content-type (first
                                                 (cstring/split
                                                   (:content-type header-map)
                                                   #"; "))
                         body (if (contains?
                                    #{(mt/text-plain)
                                      (mt/text-clojurescript)
                                      (mt/text-html)}
                                    response-content-type)
                                (let [stringified-body (String.
                                                         (byte-array
                                                           @body-bytes)
                                                         "UTF-8")]
                                  (if (= (mt/text-clojurescript)
                                         response-content-type)
                                    (read-string
                                      stringified-body)
                                    stringified-body))
                                @body-bytes)]
                     (assoc
                       header-map
                       :body
                       body))
                   header-map)]
    response))

(defn get-ssl-context
  "Creates and returns ssl context"
  [{keystore-file-path :keystore-file-path
    keystore-password :keystore-password
    keystore-type :keystore-type
    ssl-context :ssl-context}]
  (try
    (let [keystore (KeyStore/getInstance
                     (or keystore-type
                         "JKS"))
          keystore-is (FileInputStream.
                        keystore-file-path)
          pass-char-array (when (and keystore-password
                                     (string?
                                       keystore-password))
                            (char-array
                              keystore-password))
          void (.load
                 keystore
                 keystore-is
                 pass-char-array)
          tmf (TrustManagerFactory/getInstance
                (TrustManagerFactory/getDefaultAlgorithm))
          ssl-context-instance (SSLContext/getInstance
                                 (or ssl-context
                                     "TLSv1.2"))
          void (.init
                 tmf
                 keystore)
          void (.init
                 ssl-context-instance
                 nil
                 (.getTrustManagers
                   tmf)
                 nil)]
      ssl-context-instance)
    (catch Exception e
      (println
        (.getMessage
          e))
     ))
 )

(defn get-client-socket
  "Creates and returns socket to server"
  [host
   port
   & [certificate-config]]
  (let [ssl-socket-a (atom nil)]
    (try
      (when (and host
                 (string?
                   host)
                 (not
                   (cstring/blank?
                     host))
                 port
                 (number?
                   port))
        (if (and (not= port
                       80)
                 certificate-config
                 (map?
                   certificate-config))
          (if-let [ssl-context-instance (get-ssl-context
                                          certificate-config)]
            (let [ssl-socket (.createSocket
                               (.getSocketFactory
                                 ssl-context-instance)
                               host
                               port)]
              (.setEnabledProtocols
                ssl-socket
                (into-array
                  ["TLSv1"
                   "TLSv1.1"
                   "TLSv1.2"
                   "SSLv3"]))
              (reset!
                ssl-socket-a
                ssl-socket))
            (let [factory (SSLSocketFactory/getDefault)
                  ssl-socket (.createSocket
                               factory
                               host
                               port)]
              (reset!
                ssl-socket-a
                ssl-socket))
           )
          (reset!
            ssl-socket-a
            (Socket.
              host
              port))
         ))
      (catch Exception e
        (println
          (.getMessage
            e))
        ))
   @ssl-socket-a))

(defn fire-request
  "Execute request on host from passed parameters"
  [host
   port
   request-method
   uri
   & [certificate-config
      body]]
  (when-let [client-socket (get-client-socket
                             host
                             port
                             certificate-config)]
    (let [input-stream (.getInputStream
                         client-socket)
          output-stream (.getOutputStream
                          client-socket)
          [raw-request-header
           request-body] (pack-request
                           host
                           port
                           request-method
                           uri
                           body)]
      (when (and raw-request-header
                 (string?
                   raw-request-header)
                 (not
                   (cstring/blank?
                     raw-request-header))
             )
        (when (instance?
                javax.net.ssl.SSLSocket
                client-socket)
          (.startHandshake
            client-socket))
        (send-request
          output-stream
          (.getBytes
            raw-request-header
            "UTF-8")
          request-body)
        (let [processed-response (process-response
                                   input-stream)]
          (.close
            client-socket)
          processed-response))
     ))
 )

(defn url-through-stream
  "Fetch from url through stream, this works with HTTP/2.0"
  [url-string]
  (let [url (java.net.URL.
              url-string) ; eg. "https://www.google.com/"
        in (java.io.BufferedReader.
             (java.io.InputStreamReader.
               (.openStream
                 url))
            )]
    (try
      (let [in-line (atom "")
            in-content (atom "")]
        (while @in-line
          (reset!
            in-line
            (.readLine
              in))
          (swap!
            in-content
            str
            @in-line))
        @in-content)
      (catch Exception e
        (println
          e))
     )
   )
 )

(defn url-connection
  "Fetch from url connection, doesn't work with HTTP/2.0"
  [url-string]
  (let [u (java.net.URL.
            url-string) ; "http://www.java2s.com"
        uc (.openConnection
             u)
        response-headers (atom {})
        response-body (atom {})]
    (.setRequestProperty
      uc
      "Accept"
      "*/*")
    (.setRequestProperty
      uc
      "Accept-Encoding"
      "gzip, deflate, br")
    (.setRequestProperty
      uc
      "Accept-Language"
      "sr,en;q=0.5")
    (.setRequestProperty
      uc
      "Cache-Control"
      "no-cache")
    (.setRequestProperty
      uc
      "Connection"
      "keep-alive")
    (.setRequestProperty
      uc
      "Pragma"
      "no-cache")
    (.setRequestProperty
      uc
      "User-Agent"
      "Mozilla/5.0 (X11; Ubuntu; Linu…) Gecko/20100101 Firefox/66.0")
    (.connect
      uc)
    (swap!
      response-headers
      assoc
      :content-type (.getContentType
                      uc)
      :content-encoding (.getContentEncoding
                          uc)
      :content-length (.getContentLength
                        uc)
      :date (.getDate
              uc)
      :last-modified (.getLastModified
                       uc)
      :expiration (.getExpiration
                    uc))
    (let [is (.getContent
               uc)
          body-bytes (atom [])]
      (when-let [content-length (:content-length
                                  @response-headers)]
        (dotimes [i content-length]
          (let [read-byte (unchecked-byte
                            (.read
                              is))]
            (swap!
              body-bytes
              conj
              read-byte))
         ))
      (reset!
        response-body
        (String.
          (byte-array
            @body-bytes)
          "UTF-8"))
     )
    [@response-headers
     @response-body]))

