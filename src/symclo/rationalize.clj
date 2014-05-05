(ns symclo.rationalize
  (:gen-class))
(use '[clojure.core.match :only (match)])
(use 'clojure.math.numeric-tower)
(use 'clojure.tools.trace)
(require '[symclo.core :as simp])
(require '[symclo.util :as util])

(defn- third [u] (first (nnext u)))

(defn numer 
  "Gives numerator of an expression."
  
  [u]
  (match [(simp/kind u)]
         [:fracop] (fnext u)
         [:quotop] (fnext u)
         [:powop] (cond
                   (and (= (simp/kind (third u)) :number) (< (third u) 0)) 1
                   (and (= (simp/kind (third u)) :number) (>= (third u) 0)) u
                   (and (= (simp/kind (third u)) :fracop) (< (/ (fnext (third u)) (first (nnext (third u)))) 0)) 1 
                   (and (= (simp/kind (third u)) :fracop) (>= (/ (fnext (third u)) (first (nnext (third u)))) 0)) u
                   :else u)
         [:prodop] (list '* (numer (fnext u)) (numer (third u)))
         [_] u))

(defn denom 
  "Gives the denominator of a expression."
  
  [u]
  (match [(simp/kind u)]
         [:fracop] (third u)
         [:quotop] (third u)
         [:powop] (cond
                   (and (= (simp/kind (third u)) :number) (< (third u) 0)) (simp/simplify* (list '** u -1))
                   (and (= (simp/kind (third u)) :number) (>= (third u) 0)) 1
                   (and (= (simp/kind (third u)) :fracop) (< (/ (fnext (third u)) (first (nnext (third u)))) 0)) (simp/simplify* (list '** u -1))
                   (and (= (simp/kind (third u)) :fracop) (>= (/ (fnext (third u)) (first (nnext (third u)))) 0)) 1
                   :else 1)
         [:prodop] (list '* (denom (fnext u)) (denom (third u)))
         [_] 1))

(defn- natural-sum [u v]
  (let [m (simp/simplify* (numer u))
        r (simp/simplify* (denom u))
        n (simp/simplify* (numer v))
        s (simp/simplify* (denom v))]
    (if (and (= r 1) (= s 1))
      (list '+ u v)
      (list '* (natural-sum (list '* m s) (list '* n r)) (list '** (list '* r s) -1)))))

(defn natural*
  "Rationalize an expression, e.g.: 
   (natural (+ (/ 1 x) (/ 1 y)) = (/ (+ x y) (* x y))). 
   Call simplify* before and after" 
  [u] 
  (cond
   (= (simp/kind u) :powop) (list '** (natural* (fnext u)) (third u))
   (= (simp/kind u) :prodop) (list '* (natural* (fnext u)) (natural* (third u)))
   (= (simp/kind u) :sumop) (natural-sum (natural* (fnext u)) (natural* (third u)))
   :else u))

(defmacro natural 
  "Calls natural* on args. automatic simplification is implicit."
  
  [& args]
  `(map (comp simp/simplify* natural* simp/simplify*) '(~@args)))


