(ns symclo.rationalize
  (:gen-class))
(use '[clojure.core.match :only (match)])
(use 'clojure.math.numeric-tower)
(use 'clojure.tools.trace)
(require '[symclo.core :as simp])
(require '[symclo.util :as util])

(defn numer [u]
  (match [(simp/kind u)]
         [:fracop] (fnext u)
         [:quotop] (fnext u)
         [:powop] (cond
                   (and (= (simp/kind (first (nnext u))) :number) (< (first (nnext u))) 0) 1
                   (and (= (simp/kind (first (nnext u))) :number) (>= (first (nnext u)) 0)) u
                   (and (= (simp/kind (first (nnext u))) :fracop) (< (/ (fnext (first (nnext u))) (first (nnext (first (nnext u))))) 0)) (simp/simplify* (list '** u -1))
                   (and (= (simp/kind (first (nnext u))) :fracop) (>= (/ (fnext (first (nnext u))) (first (nnext (first (nnext u))))) 0)) 1
                   :else u)
         [:prodop] (list '* (numer (fnext u)) (numer (first (nnext u))))
         [_] u))

(defn denom [u]
  (match [(simp/kind u)]
         [:fracop] (nnext u)
         [:quotop] (nnext u)
         [:powop] (cond
                   (and (= (simp/kind (first (nnext u))) :number) (< (first (nnext u)) 0)) (simp/simplify* (list '** u -1))
                   (and (= (simp/kind (first (nnext u))) :number) (>= (first (nnext u)) 0)) 1
                   (and (= (simp/kind (first (nnext u))) :fracop) (< (/ (fnext (first (nnext u))) (first (nnext (first (nnext u))))) 0)) (simp/simplify* (list '** u -1))
                   (and (= (simp/kind (first (nnext u))) :fracop) (>= (/ (fnext (first (nnext u))) (first (nnext (first (nnext u))))) 0)) 1
                   :else 1)
         [:prodop] (list '* (denom (fnext u)) (denom (first (nnext u))))
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
   (natural (+ (/ 1 x) (/ 1 y)) = (/ (+ x y) (* x y)))" 
  [u] 
  (cond
   (= (simp/kind u) :powop) (list '** (natural* (fnext u)) (first (nnext u)))
   (= (simp/kind u) :prodop) (list '* (natural* (fnext u)) (natural* (first (nnext u))))
   (= (simp/kind u) :sumop) (natural-sum (natural* (fnext u)) (natural* (first (nnext u))))
   :else u))

(defmacro natural [& args]
  `(map (comp simp/simplify* natural* simp/simplify*) '(~@args)))


