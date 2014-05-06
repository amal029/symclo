(defproject symclo "0.1.0-SNAPSHOT"
  :description "A computer algebra system (CAS) library in clojure"
  :url "https://github.com/amal029/symclo"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :aot :all
  :main symclo.core
  :dependencies [[org.clojure/clojure "1.5.1"] [org.clojure/tools.trace "0.7.1"]
                 [uk.co.forward/incanter-core-jblas "1.3.0-SNAPSHOT"] 
                 [org.clojure/math.numeric-tower "0.0.4"] [org.clojure/core.typed "0.2.44"]
                 [org.clojure/clojure-contrib "1.2.0"] [com.phansen/clojure.adt "1.0.0"] [org.clojure/core.match "0.2.1"]]
  :codox {:sources ["src"]
          :src-dir-uri "http://github.com/amal029/symclo/blob/master/"
          :src-linenum-anchor-prefix "L"})
