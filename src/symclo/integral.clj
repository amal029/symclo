(ns symclo.integral
  (:gen-class))
(use '[clojure.core.match :only (match)])
(use 'clojure.math.numeric-tower)
(use 'clojure.tools.trace)
(require '[symclo.core :as simp])
(require '[symclo.expand :as expand])
(require '[symclo.trig :as trig])
(require '[symclo.derivative :as deriv])
(require '[symclo.util :as util])

(def third (comp first nnext))

;;; TODO: fill in all the functions.
(declare trial-sub)
(declare linear-props)
(declare integral-table)
(declare substitution-method)
(declare substitute)

(defn- substitution-method [f x]
  (let [P (trial-sub f)
        FS (map #(if (and (util/free-of % x) (not= % x))
                   (let [u (substitute (simp/simplify* (list '/ f (deriv/deriv* % x))) % 'v)]
                     (if (util/free-of u x)
                       (substitute (integrate* u 'v) 'v %)))) P)]
    (if (some (partial not= 'FAIL) FS)
      (first (filter (partial not= 'FAIL) FS))
      'FAIL)))

(defn integrate* [f x]
  (let [F (integral-table f x)]
    (if (= 'FAIL F) 
      (let [F (linear-props f x)]
        (if (= 'FAIL F)
          (let [F (substitution-method f x)]
            (if (= 'FAIL F)
              (let [g (expand/expand* f)]
                (if (not= g f) (integrate* g x)) F)
              ;; else
              F))
          ;; else
          F)) 
      ;; else
      F)))


(defmacro integrate [arg sym]
  `(simp/simplify* (integrate* (simp/simplify* '~arg) '~sym)))
