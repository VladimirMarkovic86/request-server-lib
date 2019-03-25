(ns request-server-lib.core-test
  (:require [clojure.test :refer :all]
            [request-server-lib.core :refer :all]
            [ajax-lib.http.entity-header :as eh]
            [ajax-lib.http.general-header :as gh]
            [ajax-lib.http.request-header :as rh]
            [ajax-lib.http.mime-type :as mt]
            [ajax-lib.http.status-code :as stc]
            [xml-lib.core :as xml]))

(deftest test-pack-request-from-map
  
  (testing "Test pack request from map"
    
    (let [request-map {}
          raw-request (pack-request-from-map
                        request-map)]
      
      (is
        (nil?
          raw-request)
       )
      
     )
    
    (let [request-map {:headers {(rh/accept) (mt/text-clojurescript)}}
          raw-request (pack-request-from-map
                        request-map)]
      
      (is
        (nil?
          raw-request)
       )
      
     )
    
    (let [request-map {:request-start-line ""
                       :headers {(rh/accept) (mt/text-clojurescript)}}
          raw-request (pack-request-from-map
                        request-map)]
      
      (is
        (nil?
          raw-request)
       )
      
     )
    
    (let [request-map {:request-start-line "POST / HTTP/1.1"}
          raw-request (pack-request-from-map
                        request-map)]
      
      (is
        (nil?
          raw-request)
       )
      
     )
    
    (let [request-map {:request-start-line "POST / HTTP/1.1"
                       :headers {}}
          raw-request (pack-request-from-map
                        request-map)]
      
      (is
        (nil?
          raw-request)
       )
      
     )
    
    (let [request-map {:request-start-line "POST / HTTP/1.1"
                       :headers {(rh/accept) (mt/text-clojurescript)}}
          raw-request (pack-request-from-map
                        request-map)]
      
      (is
        (= raw-request
           (str
             "POST / HTTP/1.1\r\n"
             "Accept: text/clojurescript\r\n"))
       )
      
     )
    
    (let [test-user-agent "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:61.0) Gecko/20100101 Firefox/61.0"
          request-map {:request-start-line "POST / HTTP/1.1"
                       :headers (sorted-map
                                  (rh/accept) (mt/text-clojurescript)
                                  (rh/accept-encoding) "gzip, deflate, br"
                                  (rh/accept-language) "sr,en;q=0.5"
                                  (gh/cache-control)	"no-cache"
                                  (gh/connection) "keep-alive"
                                  (eh/content-type) (mt/text-clojurescript)
                                  (rh/host) "ide:1604"
                                  (rh/origin) "https://ide:1614"
                                  (gh/pragma) "no-cache"
                                  (rh/referer) "https://ide:1614/"
                                  (rh/user-agent) test-user-agent)}
          raw-request (pack-request-from-map
                        request-map)]
      (is
        (= raw-request
           (str
             "POST / HTTP/1.1\r\n"
             "Accept: text/clojurescript\r\n"
             "Accept-Encoding: gzip, deflate, br\r\n"
             "Accept-Language: sr,en;q=0.5\r\n"
             "Cache-Control: no-cache\r\n"
             "Connection: keep-alive\r\n"
             "Content-Type: text/clojurescript\r\n"
             "Host: ide:1604\r\n"
             "Origin: https://ide:1614\r\n"
             "Pragma: no-cache\r\n"
             "Referer: https://ide:1614/\r\n"
             "User-Agent: " test-user-agent "\r\n"))
       )
      
     )
    
   )
  
 )

(deftest test-pack-request
  
  (testing "Test pack request"
    
    (let [host nil
          port nil
          request-method nil
          uri nil
          body nil
          packed-request (pack-request
                           host
                           port
                           request-method
                           uri
                           body)]
      
      (is
        (nil?
          packed-request)
       )
     
     )
    
    (let [host "ide"
          port 1604
          request-method "POST"
          uri "/"
          body nil
          [raw-request-headers
           request-body] (pack-request
                           host
                           port
                           request-method
                           uri
                           body)]
      
      (is
        (and (= raw-request-headers
                (str
                  "POST / HTTP/1.1\r\n"
                  "Accept: */*\r\n"
                  "Accept-Encoding: gzip, deflate, br\r\n"
                  "Accept-Language: sr,en;q=0.5\r\n"
                  "Cache-Control: no-cache\r\n"
                  "Connection: keep-alive\r\n"
                  "Host: ide:1604\r\n"
                  "Origin: https://ide:1604\r\n"
                  "Pragma: no-cache\r\n"
                  "Referer: https://ide:1604/\r\n"
                  "User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:61.0) Gecko/20100101 Firefox/61.0\r\n\r\n"))
             (nil?
               request-body))
       )
     
     )
    
   )
  
 )

(deftest test-get-ssl-context
  
  (testing "Test get ssl context"
    
    (let [ssl-context-instance (get-ssl-context
                                 nil)]
      
      (is
        (nil?
          ssl-context-instance)
       )
      
     )
    
    (let [ssl-context-instance (get-ssl-context
                                 {})]
      
      (is
        (nil?
          ssl-context-instance)
       )
      
     )
    
    (let [ssl-context-instance (get-ssl-context
                                 {:keystore-file-path "./resources/certificate/request_server_lib.jks"})]
      
      (is
        (nil?
          ssl-context-instance)
       )
      
     )
    
    (let [ssl-context-instance (get-ssl-context
                                 {:keystore-password "ultras12"})]
      
      (is
        (nil?
          ssl-context-instance)
       )
      
     )
    
    (let [ssl-context-instance (get-ssl-context
                                 {:keystore-file-path "./resources/certificate/request_server_lib.jks"
                                  :keystore-password "ultras12"})]
      
      (is
        (and (not
               (nil?
                 ssl-context-instance))
             (instance?
               javax.net.ssl.SSLContext
               ssl-context-instance))
       )
      
     )
    
    (let [ssl-context-instance (get-ssl-context
                                 {:keystore-file-path "./resources/certificate/request_server_lib.jks"
                                  :keystore-password "ultras12"
                                  :keystore-type nil
                                  :ssl-context nil})]
      
      (is
        (and (not
               (nil?
                 ssl-context-instance))
             (instance?
               javax.net.ssl.SSLContext
               ssl-context-instance))
       )
      
     )
    
    (let [ssl-context-instance (get-ssl-context
                                 {:keystore-file-path "./resources/certificate/google_keystore.jks"
                                  :keystore-password "ultras12"
                                  :keystore-type nil
                                  :ssl-context nil})]
      
      (is
        (and (not
               (nil?
                 ssl-context-instance))
             (instance?
               javax.net.ssl.SSLContext
               ssl-context-instance))
       )
      
     )
    
   )
  
 )

(deftest test-get-client-socket
  
  (testing "Test get client socket"
    
    (let [host nil
          port nil
          certificate-config nil
          client-socket (get-client-socket
                          host
                          port
                          certificate-config)]
      
      (is
        (nil?
          client-socket)
       )
      
     )
    
    (let [host "ide"
          port nil
          certificate-config nil
          client-socket (get-client-socket
                          host
                          port
                          certificate-config)]
      
      (is
        (nil?
          client-socket)
       )
      
     )
    
    (let [host nil
          port 1604
          certificate-config nil
          client-socket (get-client-socket
                          host
                          port
                          certificate-config)]
      
      (is
        (nil?
          client-socket)
       )
      
     )
    
    (let [host ""
          port 1604
          certificate-config nil
          client-socket (get-client-socket
                          host
                          port
                          certificate-config)]
      
      (is
        (nil?
          client-socket)
       )
      
     )
    
    (let [host "ide"
          port 1604
          certificate-config nil
          client-socket (get-client-socket
                          host
                          port
                          certificate-config)]
      
      (is
        (and (not
               (nil?
                 client-socket))
             (instance?
               java.net.Socket
               client-socket))
       )
      
     )
    
    (let [host "ide"
          port 1604
          certificate-config {:keystore-file-path "./resources/certificate/request_server_lib.jks"
                              :keystore-password "ultras12"
                              :keystore-type nil
                              :ssl-context nil}
          client-socket (get-client-socket
                          host
                          port
                          certificate-config)]
      
      (is
        (and (not
               (nil?
                 client-socket))
             (instance?
               sun.security.ssl.SSLSocketImpl
               client-socket))
       )
      
     )
    
   )
  
 )

(deftest test-send-request
  
  (testing "Test send request"
    
    (let [output-stream nil
          raw-request-header nil
          request-header nil
          raw-request-body nil
          request-body raw-request-body]
      
      (is
        (nil?
          (send-request
            output-stream
            request-header
            request-body))
       )
      
     )
    
    (let [host "sample"
          port 1603
          certificate-config {:keystore-file-path "./resources/certificate/sample_server.jks"
                              :keystore-password "ultras12"
                              :keystore-type nil
                              :ssl-context nil}
          client-socket (get-client-socket
                          host
                          port
                          certificate-config)
          output-stream (.getOutputStream
                          client-socket)
          raw-request-header (str
                               "GET / HTTP/1.1\r\n"
                               "Accept: */*\r\n"
                               "Accept-Encoding: gzip, deflate, br\r\n"
                               "Accept-Language: sr,en;q=0.5\r\n"
                               "Cache-Control: no-cache\r\n"
                               "Connection: keep-alive\r\n"
                               "Host: sample:1603\r\n"
                               "Origin: https://sample:1603/\r\n"
                               "Pragma: no-cache\r\n"
                               "Referer: https://sample:1603/\r\n"
                               "User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:61.0) Gecko/20100101 Firefox/61.0\r\n\r\n")
          request-header (.getBytes
                           raw-request-header
                           "UTF-8")
          raw-request-body nil
          request-body raw-request-body]
      
      (is
        (not
          (nil?
            (send-request
              output-stream
              request-header
              request-body))
         )
       )
      
      ; Wait for host to answer
      (Thread/sleep 100)
      
     )
    
    (let [host "sample"
          port 1603
          certificate-config {:keystore-file-path "./resources/certificate/sample_server.jks"
                              :keystore-password "ultras12"
                              :keystore-type nil
                              :ssl-context nil}
          client-socket (get-client-socket
                          host
                          port
                          certificate-config)
          output-stream (.getOutputStream
                          client-socket)
          raw-request-body "Test body"
          request-body (.getBytes
                         raw-request-body
                         "UTF-8")
          raw-request-header (str
                               "GET / HTTP/1.1\r\n"
                               "Accept: */*\r\n"
                               "Accept-Encoding: gzip, deflate, br\r\n"
                               "Accept-Language: sr,en;q=0.5\r\n"
                               "Cache-Control: no-cache\r\n"
                               "Connection: keep-alive\r\n"
                               "Content-Length: " (count request-body) "\r\n"
                               "Content-Type: text/plain\r\n"
                               "Host: sample:1603\r\n"
                               "Origin: https://sample:1603/\r\n"
                               "Pragma: no-cache\r\n"
                               "Referer: https://sample:1603/\r\n"
                               "User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:61.0) Gecko/20100101 Firefox/61.0\r\n\r\n")
          request-header (.getBytes
                           raw-request-header
                           "UTF-8")]
      
      (is
        (not
          (nil?
            (send-request
              output-stream
              request-header
              request-body))
         )
       )
      
      ; Wait for host to answer
      (Thread/sleep 100)
      
     )
    
   )
  
 )

(deftest test-process-response
  
  (testing "Test process response"
    
    (let [host "sample"
          port 1613
          certificate-config {:keystore-file-path "./resources/certificate/sample_client.jks"
                              :keystore-password "ultras12"
                              :keystore-type nil
                              :ssl-context nil}
          client-socket (get-client-socket
                          host
                          port
                          certificate-config)
          output-stream (.getOutputStream
                          client-socket)
          input-stream (.getInputStream
                         client-socket)
          raw-request-header (str
                               "GET / HTTP/1.1\r\n"
                               "Accept: */*\r\n"
                               "Accept-Encoding: gzip, deflate, br\r\n"
                               "Accept-Language: sr,en;q=0.5\r\n"
                               "Cache-Control: no-cache\r\n"
                               "Connection: keep-alive\r\n"
                               "Host: sample:1613\r\n"
                               "Pragma: no-cache\r\n"
                               "Referer: https://sample:1613/\r\n"
                               "User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:61.0) Gecko/20100101 Firefox/61.0\r\n\r\n")
          request-header (.getBytes
                           raw-request-header
                           "UTF-8")
          raw-request-body nil
          request-body raw-request-body
          void (send-request
                 output-stream
                 request-header
                 request-body)
          response (process-response
                     input-stream)
          response-body-parsed (xml/html-parse
                                 (:body response))]
      
      (is
        (= (read-string
             (:status-code response))
           (stc/ok))
       )
      
      (is
        (= (:server response)
           "cljserver")
       )
      
      (is
        (= (:content-type response)
           "text/html")
       )
      
      (let [html-content (:content response-body-parsed)
            head-content (:content
                           (get
                             html-content
                             0))
            title-content (:content
                            (get
                               head-content
                               2))
            title-content-elem (first
                                 title-content)]
        (is
          (= title-content-elem
             "Sample")
         )
        
       )
      
     )
    
   )
  
 )

(deftest test-fire-request
  
  (testing "Test fire request"
    
    (let [host "sample"
          port 1603
          request-method "POST"
          request-uri "/get-labels"
          certificate-config {:keystore-file-path "./resources/certificate/sample_server.jks"
                              :keystore-password "ultras12"
                              :keystore-type nil
                              :ssl-context nil}
          request-body nil
          processed-reponse (fire-request
                              host
                              port
                              request-method
                              request-uri
                              certificate-config
                              request-body)]
      (is
        (= (get-in
             processed-reponse
             [:body
              :status])
           "success")
       )
     )
    
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
  
 )

