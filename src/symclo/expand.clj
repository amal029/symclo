(ns symclo.expand
  (:gen-class))
(use '[clojure.core.match :only (match)])
(use 'clojure.math.numeric-tower)
(use 'clojure.tools.trace)
(require '[symclo.core :as simp])

;;; Forward declarations
(declare expand-product)
(declare expand-power)
(declare expand*)

;;; Distributes multiplication over sums
(defn expand* [u]
  (cond
   (= (simp/kind u) :sumop) 
   (let [[_ v t] u]
     (list '+ (expand* v) (expand* t)))
   (= (simp/kind u) :prodop) 
   (let [[_ v t] u]
     (expand-product (expand* v) (expand* t)))
   (= (simp/kind u) :powop)
   (let [b (simp/base u)
         e (simp/exponent u)]
     (if (= (simp/kind e) :number)
       (when (>= e 2) (expand-power (expand* b) e))
       u))
   :else u))

(defn- expand-product [r s]
  (cond
   (= (simp/kind r) :sumop) 
   (if (= (count r) 3)
     (let [[_ f t] r] 
       (list '+ (expand-product f s) (expand-product t s)))
     (list '* r s))
   (= (simp/kind s) :sumop) (expand-product s r)
   :else (list '* r s)))

(defn- expand-power [u n]
  (if (and (integer? n) (>= n 0))
    (cond
     (= (simp/kind u) :sumop)
     (if (= (count u) 3)
       (let [[ _ f r] u]
         (loop [k 0
                s 0]
           (if (<= k n)
             (let [c (/ (simp/factorial n) (* (simp/factorial k) (simp/factorial (- n k))))
                   s (list '+ s (expand-product (list '* c (list '** f (- n k)))
                                                (expand-power r k)))]
               (recur (+ k 1) s))
             s)))
       (list '** u n))
     :else (list '** u n))
    (list '** u n)))

(defmacro expand [& args]
  `(map (comp simp/simplify* expand* simp/simplify*) '(~@args)))
