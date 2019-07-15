(defproject org.clojars.vladimirmarkovic86/request-server-lib "0.1.8"
  :description "Request server library"
  :url "http://github.com/VladimirMarkovic86/request-server-lib"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojars.vladimirmarkovic86/utils-lib "0.4.12"]
                 [org.clojars.vladimirmarkovic86/ajax-lib "0.1.13"]
                 [org.clojars.vladimirmarkovic86/xml-lib "0.1.2"]
                 ]

  :min-lein-version "2.0.0"
  
  :source-paths ["src/clj"]
  :test-paths ["test/clj"]
  
  :jar-exclusions [#"README.md$"
                   #"LICENSE$"])

