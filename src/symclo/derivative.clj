(ns symclo.derivative
  (:gen-class))
(use '[clojure.core.match :only (match)])
(use 'clojure.math.numeric-tower)
(use 'clojure.tools.trace)
(require '[symclo.core :as simp])
(require '[symclo.trig :as trig])
(require '[symclo.util :as util])

(defn- third [x] (first (nnext x)))

(defn deriv* 
  "u is the function, x is the symbol against which derivative needs to
  be done. Call simplify* before and after deriviation."  
  [u x]
  (if (= (symbol? x))
    (match [(simp/kind u)]
           [:symbol] (if (= x u) 1 (list '%deriv (list u x)))
           [:powop] (let [[_ v w] u]
                      (cond 
                       true ;; (or (= (simp/kind w) :fracop) (= (simp/kind w) :number))
                       (simp/simplify* (list '+ (list '* (list '* w (list '** v (list '- w 1))) (deriv* v x)) 
                                             (list '* (list '* (deriv* w x) (list '** v w)) (list '%ln v))))
                       :else (list '%deriv (list u x))))
           [:sumop] (simp/simplify* (list '+ (deriv* (second u) x) (deriv* (third u) x)))
           [:prodop] (simp/simplify* (list '+ (list '* (third u) (deriv* (second u) x)) (list '* (second u) (deriv* (third u) x))))
           [:function] (cond
                        (= (trig/trig-kind u) :sin) (simp/simplify* (list '* (list 'cos (second u)) (deriv* (second u) x)))
                        (= (trig/trig-kind u) :cos) (simp/simplify* (list '* (list '* -1 (list 'sin (second u))) (deriv* (second u) x)))
                        :else (list '%deriv (list u x)))
           [_] (cond 
                (util/free-of u x) 0
                :else (list '%deriv (list u x))))
    (list '%deriv (list u x))))

(defmacro deriv 
  "Calls deriv* on arg w.r.t sym. autosimplification is implicit."
  
  [arg sym]
  `(simp/simplify* (deriv* (simp/simplify* '~arg) '~sym)))
