(ns symclo.ode
  (:gen-class))
(use '[clojure.core.match :only (match)])
(use 'clojure.tools.trace)
(require '[symclo.core :as simp])
(require '[symclo.util :as util])
(require '[symclo.derivative :as deriv])
(require '[symclo.integral :as integral])
(require '[symclo.rationalize :as natural])


(defn- transform-ode [w x y]
  (let [v (natural/rational-simplify* ((comp simp/simplify* natural/natural* simp/simplify*) (list '- (first w) (second w))) [x y])
        n ((comp simp/simplify* natural/numer) v)]
    [(util/coefficient-polynomial-gpe n (list '%deriv (list y x)) 0)
     (util/coefficient-polynomial-gpe n (list '%deriv (list y x)) 1)]))

;;; TODO
(defn- separable-ode [m n x y]
  'FAIL)

(defn- solve-exact [m n x y]
  (cond 
   (= n 0) 'FAIL
   (= m 0) (simp/simplify* (list '- y '%C))
   :else
   (let [d (simp/simplify* (list '- (deriv/deriv* m y) (deriv/deriv* n x)))
         [u d] (if (= d 0) [1 d]
                   (let [F (natural/rational-simplify* ((comp simp/simplify* natural/natural*) (list '/ d n)))
                         G (natural/rational-simplify* ((comp simp/simplify* natural/natural*) (list '/ (list '* -1 d) m)))]
                     (cond 
                      (util/free-of F y) [(list '** '%e (integral/integrate* F x)) 0]
                      (util/free-of G x) [(list '** '%e (integral/integrate* G y)) 0]
                      :else [nil d])))]
     (cond
      (= d 0) (let [g (integral/integrate* (simp/simplify* (list '* u m)) x)
                    hp (simp/simplify* (list '- (list '* u n) (deriv/deriv* g y)))
                    h (integral/integrate* hp y)]
                (simp/simplify* (list '- (list '+ g h) '%C)))
      :else 'FAIL))))

(defn- solve-ode
  [w x y]
  (let [p (transform-ode w x y)
        m (first p)
        n (second p)
        f (separable-ode m n x y)
        f (if (= f 'FAIL)
          (solve-exact m n x y)
          f)]
    f))

