(ns symclo.integral
  (:gen-class))
(use '[clojure.core.match :only (match)])
(use 'clojure.math.numeric-tower)
(use 'clojure.tools.trace)
(require '[symclo.core :as simp])
(require '[symclo.trig :as trig])
(require '[symclo.util :as util])

(defn- third [x] (first (nnext x)))

