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
(require '[symclo.rationalize :as natural])

(def ^:private third (comp first nnext))

(declare trial-sub)
(declare linear-props)
(declare integral-table)
(declare substitution-method)
(declare integrate*)
(declare integrate-rational-form)

;;; Add more definition here
(defn- integral-table [f x]
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

(defn- linear-props [f x]
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
(defn- trial-sub [f x]
    (filter #(not (util/free-of % x)) (set (util/complete-sub-expression f))))

(defn- substitution-method [f x]
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

(defn- quadratic? 
  "Checks if u is quadratic w.r.t x"
  [u x]
  (= (util/degree-polynomial u x) 2))

(defn- linear? 
  "Checks if u is linear w.r.t x"
  [u x]
  (= (util/degree-polynomial u x) 1))

(defn- integrate-rational-form [u x]
  (cond
   ;; The first case of induction
   (and (= (natural/numer u) 1) (quadratic? (natural/denom u) x))
   (let [a (util/coefficient-polynomial-gpe (natural/denom u) x 2)
         b (util/coefficient-polynomial-gpe (natural/denom u) x 1)
         c (util/coefficient-polynomial-gpe (natural/denom u) x 0)
         vv (simp/simplify* (list '- (list '** b 2) (list '* 4 a c)))]
     (if (or (= (simp/kind vv) :fracop) (= (simp/kind vv) :number))
       (cond
        (= vv 0)
        (let [two-ax-plus-b (simp/simplify* (list '+ (list '* 2 a x) b))]
          (simp/simplify* (list '* -1 (list '/ 2 two-ax-plus-b))))
        (> vv 0)
        (let [b2-minus-4ac-sqrt (simp/simplify* (list '** (list '- (list '** b 2) (list '* 4 a c)) (list '/ 1 2)))
              two-ax-plus-b (simp/simplify* (list '+ (list '* 2 a x) b))]
          (simp/simplify* (list '* 2 
                                (list '/ 
                                      (list 'arctanh
                                            (list '/ two-ax-plus-b b2-minus-4ac-sqrt)) 
                                      b2-minus-4ac-sqrt))))
        :else
        (let [four-ac-minus-b2-sqrt (simp/simplify* (list '** (list '- (list '* 4 a c) (list '** b 2)) (list '/ 1 2)))
              two-ax-plus-b (simp/simplify* (list '+ (list '* 2 a x) b))]
          (simp/simplify* (list '* 2 
                                (list '/ 
                                      (list 'arctan 
                                            (list '/ two-ax-plus-b four-ac-minus-b2-sqrt)) 
                                      four-ac-minus-b2-sqrt)))))
       ;; else (b^2 - 4ac) is not a number or fracop
       (let [four-ac-minus-b2-sqrt (simp/simplify* (list '** (list '- (list '* 4 a c) (list '** b 2)) (list '/ 1 2)))
             two-ax-plus-b (simp/simplify* (list '+ (list '* 2 a x) b))]
         (simp/simplify* (list '* 2 
                               (list '/ 
                                     (list 'arctan 
                                           (list '/ two-ax-plus-b four-ac-minus-b2-sqrt)) 
                                     four-ac-minus-b2-sqrt))))))

   ;; inductive case with linear numerator form
   (and (linear? (natural/numer u) x) (quadratic? (natural/denom u) x))
   (let [r (util/coefficient-polynomial-gpe (natural/numer u) x 1)
         s (util/coefficient-polynomial-gpe (natural/numer u) x 0)
         a (util/coefficient-polynomial-gpe (natural/denom u) x 2)
         b (util/coefficient-polynomial-gpe (natural/denom u) x 1)
         c (util/coefficient-polynomial-gpe (natural/denom u) x 0)
         alpha (simp/simplify* (list '/ r (list '* 2 a)))
         beta (simp/simplify* (list '- s (list '/ (list '* r b) (list '* 2 a))))]
     (simp/simplify* (list '+ 
                           (list '* alpha (list '%ln (natural/denom u)))
                           (list '* beta (integrate-rational-form (simp/simplify* (list '/ 1 (natural/denom u))))))))
   ;; Add the induction cases when the denominator is a power type
   (and (= (natural/numer u) 1) (simp/kind (natural/denom u) :powop))
   (let [[_ udb udp] (natural/denom u)]
     (if (and (integer? udp) (quadratic? udb))
       ;; then
       (let [a (util/coefficient-polynomial-gpe (natural/denom u) x 2)
             b (util/coefficient-polynomial-gpe (natural/denom u) x 1)
             c (util/coefficient-polynomial-gpe (natural/denom u) x 0)
             b2-minus-4ac (simp/simplify* (list '- (list '** b 2) (list '* 4 a c)))
             two-ax-plus-b (simp/simplify* (list '+ (list '* 2 a x) b))]
         (simp/simplify* (list '+ 
                               (list '/ 
                                     (list '* -1 two-ax-plus-b)
                                     (list '* (dec udp) b2-minus-4ac (list '** udb (dec udp))))
                               (list '/ 
                                     (list '* -1 a (- (* 4 udp) 6))
                                     (list '* (dec udp) b2-minus-4ac
                                           (integrate-rational-form (simp/simplify* (list '** udb (dec udp)))))))))
       ;; else
       'FAIL))
   (and (linear? (natural/numer u) x) (simp/kind (natural/denom u) :powop))
   (let [[_ udb udp] (natural/denom u)]
     (if (and (integer? udp) (quadratic? udb))
       ;; then
       (let [r (util/coefficient-polynomial-gpe (natural/numer u) x 1)
             s (util/coefficient-polynomial-gpe (natural/numer u) x 0)
             a (util/coefficient-polynomial-gpe (natural/denom u) x 2)
             b (util/coefficient-polynomial-gpe (natural/denom u) x 1)
             c (util/coefficient-polynomial-gpe (natural/denom u) x 0)
             minus-b-r-plus-2-as (simp/simplify* (list '+ (list '* -1 b r) (list '* 2 a s)))]
         (simp/simplify* (list '+
                               (list '/ 
                                     (list '* -1 r)
                                     (list '* 2 (dec udp) a (list '** udb (dec udp))))
                               (list '* 
                                     (list '/ minus-b-r-plus-2-as (list '* 2 a))
                                     (integrate-rational-form (simp/simplify* 
                                                               (list '/ 1 (list '** udb udp))))))))
       ;; else
       'FAIL))
   :else 'FAIL))

(defn integrate* 
  "Integrate a function f w.r.t symbol x. Call simplify* before and
   after integration."
  
  [f x]
  (let [F (integral-table f x)]
    (if (= 'FAIL F) 
      (let [F (linear-props f x)]
        (if (= 'FAIL F)
          (let [F (substitution-method f x)]
            (if (= 'FAIL F)
              (let [g (simp/simplify* (expand/expand* f))]
                (if (not= g f) 
                  (let [F (integrate* g x)]
                    (if = 'FAIL F) (integrate-rational-form f x) F) 
                  (integrate-rational-form f x)))
              ;; else
              F))
          ;; else
          F)) 
      ;; else
      F)))


(defmacro integrate 
  "Calls integrate* on arg w.r.t sym with implicit auto simplification."
  
  [arg sym]
  `(simp/simplify* (integrate* (simp/simplify* '~arg) '~sym)))
