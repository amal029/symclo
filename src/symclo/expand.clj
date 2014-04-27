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

(defn- third [x] (first (nnext x)))

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
       (if (>= e 2) (expand-power (expand* b) e) u)
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
    (expand* (reduce #(list '* % %2) (repeat n u)))
    (list '** u n)))

(defn expand-main-op [u]
  (cond
   (= (simp/kind u) :sumop) u
   (= (simp/kind u) :prodop) (expand-product (second u) (third u))
   (= (simp/kind u) :powop) (let [p (reduce #(list '* % %2) (repeat (third u) (second u)))]
                              (expand-product (second p) (third p)))
   :else u))

(defmacro expand [& args]
  `(map (comp simp/simplify* expand* simp/simplify*) '(~@args)))
