(ns symclo.util
  (:gen-class))
(use '[clojure.core.match :only (match)])
(use 'clojure.math.numeric-tower)
(use 'clojure.tools.trace)
(require '[symclo.core :as simp])

(defn is-addition? [[_ x y :as v]]
  (cond
   (= (simp/kind v) :sumop)
   (cond
    (not (or (= (simp/kind x) :prodop) (= (simp/kind y) :prodop))) true
    (and (= (simp/kind x) :prodop) (= (simp/kind y) :prodop))
    (let [[_ x z] x
          [_ y z] y]
      (cond
       (and (not (or (= (simp/kind x) :number) (= (simp/kind x) :fracop)))
            (not (or (= (simp/kind y) :number) (= (simp/kind y) :fracop)))) 
       true
       ;; x is a number > 0 and y is not a number or fraction then
       (and (or (= (simp/kind x) :number)) (> x 0) (or (= (simp/kind y) :number) (= (simp/kind y) :fracop))) true
       ;; x is a fraction > 0 and y is not a number or fraction then
       (and (or (= (simp/kind x) :fracop)) (or (= (simp/kind y) :number) (= (simp/kind y) :fracop)))
       (let [x (/ (let [[_ n _] x]) (let [[_ _ d] x]))]
         (> x 0))
       ;; y is a number > 0 and x is not a number or fraction then
       (and (or (= (simp/kind y) :number)) (> y 0) (or (= (simp/kind x) :number) (= (simp/kind x) :fracop))) true
       ;; y is a fraction > 0 and x is not a number or fraction then
       (and (or (= (simp/kind y) :fracop)) (or (= (simp/kind x) :number) (= (simp/kind x) :fracop)))
       (let [y (/ (let [[_ n _] y]) (let [[_ _ d] y]))]
         (> y 0))))
    ;; x is a product and y is not a product
    (and (= (simp/kind x) :prodop) (not (= (simp/kind y) :prodop)))
    (let [[_ x z] x]
      (cond
       (and (not (or (= (simp/kind x) :number) (= (simp/kind x) :fracop)))) 
       true
       ;; x is a number > 0 and y is not a number or fraction then
       (and (or (= (simp/kind x) :number)) (> x 0)) true
       ;; x is a fraction > 0 and y is not a number or fraction then
       (and (or (= (simp/kind x) :fracop)) (or (= (simp/kind y) :number) (= (simp/kind y) :fracop)))
       (let [x (/ (let [[_ n _] x]) (let [[_ _ d] x]))]
         (> x 0))))
    ;; y is a product and x is not a product
    (and (= (simp/kind y) :prodop) (not (= (simp/kind x) :prodop)))
    (let [[_ y z] y]
      (cond
       (and (not (or (= (simp/kind y) :number) (= (simp/kind y) :fracop)))) 
       true
       ;; y is a number > 0 and x is not a number or fraction then
       (and (or (= (simp/kind y) :number)) (> y 0)) true
       ;; y is a fraction > 0 and x is not a number or fraction then
       (and (or (= (simp/kind y) :fracop)))
       (let [y (/ (let [[_ n _] y]) (let [[_ _ d] y]))]
         (> y 0))))
    :else false)
   :else false))


;;; Adhering to the definition of simplification operator in J. Cohen text
;;;  (+ c (+ a b)) gets converted to (+ a b c)
;;;  (* c (* a b)) gets converted to (* a b c)
(defn complete-sub-expression [u]
  (cond
   (or (= (simp/kind u) :fracop) (= (simp/kind u) :symbol) (= (simp/kind u) :number)) 
   (list u)
   :else
   (let [u (cond 
            (= (simp/kind u) :sumop) (cons (first u) (simp/get-sum-operands (rest u))) 
            (= (simp/kind u) :prodop) (cons (first u) (simp/get-prod-operands (rest u))) 
            :else u)]
     (reduce #(into (complete-sub-expression %2) %) (list u) (rest u)))))

(defn substitute [f x y]
  (cond
   (= (simp/simplify* f) (simp/simplify* x)) (simp/simplify* y)
   (or (= (simp/kind f) :fracop) (= (simp/kind f) :symbol) (= (simp/kind f) :number)) 
   f
   (= (simp/kind f) :prodop) 
   (let [operands (simp/get-prod-operands (rest f))
         operands (map #(substitute % x y) operands)]
     (reduce #(list '* % %2) operands))
   (= (simp/kind f) :sumop) 
   (let [operands (simp/get-sum-operands (rest f))
         operands (map #(substitute % x y) operands)]
     (reduce #(list '+ % %2) operands))
   :else
   (cons (first f) (map #(substitute % x y) (rest f)))))

(defn free-of [u x]
  (not (some (partial = x) (complete-sub-expression u))))
