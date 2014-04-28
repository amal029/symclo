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

(declare trial-sub)
(declare linear-props)
(declare integral-table)
(declare substitution-method)
(declare integrate*)

;;; Add more definition here
(defn integral-table [f x]
  (cond
   (util/free-of f x) (list '* f x)
   (= (simp/kind f) :symbol) (simp/simplify* (list '/ (list '** x (list '+ 1 1)) (list '+ 1 1)))
   (= (simp/kind f) :powop)
   (match [(vec (flatten f))]
          [['** %e (y :guard [(partial = x)])]] f
          [['** %ln (y :guard [(partial = x)])]] (simp/simplify* (list '- (list '* x f) x))
          [['** y -1]] (if (= y x) (list '%ln x) 'FAIL)
          [['** (t :guard [(partial = x)]) (y :guard [integer?])]] (simp/simplify* (list '/ (list '** x (list '+ y 1)) (list '+ y 1)))
          [['** y (t :guard [(partial = x)])]] (if (util/free-of y x) (simp/simplify* (list '/ f (list '%ln y))) 'FAIL)
          [_] 'FAIL)
   (= (simp/kind f) :prodop)
   (match [(vec (flatten f))]
          [['* 'sec (a1 :guard [(partial = x)]) 
            'tan (a2 :guard [(partial = x)])]] (list 'sec x)
            [['* 'cot (a1 :guard [(partial = x)]) 
              'csc (a2 :guard [(partial = x)])]] (list '* -1 (list 'csc x))
              [_] 'FAIL)
   :else
   (do 
     (match [(trig/trig-kind f)]
            [:sin] (if (= (second f) x) (list '* -1 (list 'cos x)) 'FAIL)
            [:cos] (if (= (second f) x) (list 'sin x) 'FAIL)
            [:cot] (if (= (second f) x) (list '%ln (list 'sin x)) 'FAIL)
            [:tan] (if (= (second f) x) (list '%ln (list 'sec x)) 'FAIL)
            [:sec] (if (= (second f) x) (list '%ln (list '+ (list 'tan x) (list 'sec x)) 'FAIL))
            [:csc] (if (= (second f) x) (list '* -1 (list '%ln (list '+ (list 'csc x) (list 'cot x)))) 'FAIL)
            [_] 'FAIL))))

(defn linear-props [f x]
  (cond
   (= (simp/kind f) :prodop)
   (let [operands (simp/get-prod-operands (rest f))
         fx (filter #(util/free-of % x) operands)
         nfx (filter #(not (util/free-of % x)) operands)]
     (cond
      (empty? fx) 'FAIL
      (not= empty? nfx) 
      (let [res (integrate* (reduce #(list '* % %2) nfx) x)]
        (if (not= res 'FAIL) (simp/simplify* (list '* (reduce #(list '* % %2) fx) res)) 'FAIL))
      :else 'FAIL))
   (= (simp/kind f) :sumop)
   (let [res (map #(integrate* % x) (rest f))]
     (if-not (some (partial = 'FAIL) res) (simp/simplify* (reduce #(list '+ % %2) res)) 'FAIL))
   :else 'FAIL))

;;; Shortcut method for trial-sub (gets more than just the required elements)
;;; FIXME: will lead to more recursion then necessary
(defn trial-sub [f x]
    (filter #(not (util/free-of % x)) (set (util/complete-sub-expression f))))

(defn substitution-method [f x]
  (let [P (trial-sub f x)
        FS (map #(if (and (not (util/free-of % x)) (not= % x))
                   (let [u (simp/simplify* (util/substitute (simp/simplify* (list '/ f (deriv/deriv* % x))) % 'v))]
                     (if (util/free-of u x)
                       (simp/simplify* (util/substitute (integrate* u 'v) 'v %))
                       'FAIL))
                   'FAIL) P)]
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
              (let [g (simp/simplify* (expand/expand* f))]
                (if (not= g f) (integrate* g x) F))
              ;; else
              F))
          ;; else
          F)) 
      ;; else
      F)))


(defmacro integrate [arg sym]
  `(simp/simplify* (integrate* (simp/simplify* '~arg) '~sym)))
