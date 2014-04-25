;;; Expert system based simplification of trignometric functions 
;;; See: Automated and readable simplification of trignometric
;;; expressions
;;; Author: Avinash Malik
;;; Sun Apr 20 13:18:50 NZST 2014

(ns symclo.trig
  (:gen-class))
(use '[clojure.core.match :only (match)])
(use 'clojure.math.numeric-tower)
(use 'clojure.tools.trace)
(require '[symclo.core :as simp])
(require '[symclo.util :as util])
(require '[symclo.expand :as expand])
(require '[symclo.rationalize :as natural])

(declare contract-trig-rules)
(declare contract-trig-product)
(declare contract-trig-power)

(defn- third [x] (first (nnext x)))

(defn trig-kind [op]
  (if (seq? op)
    (cond 
     (= (first op) 'sin) :sin
     (= (first op) 'cos) :cos
     (= (first op) 'tan) :tan
     (= (first op) 'cot) :cot
     (= (first op) 'sec) :sec
     (= (first op) 'csc) :csc
     :else :other)
    :other))

;;; The trignometric identities from text books trig-functions can only
;;; ever have 1 operand

(defn tr1 [[_ oo :as v]]
  (match [(trig-kind v)]
         [:sec] (list '/ 1 (list 'cos oo))
         [:csc] (list '/ 1 (list 'sin oo))
         [_] v))

(defn tr2 [[_ oo :as v]]
  (match [(trig-kind v)]
         [:tan] (list '/ (list 'sin oo) (list 'cos oo))
         [:cot] (list '/ (list 'cos oo) (list 'sin oo))
         [_] v))

;;; FIXME: we can make this stronger
(defn tr3 [[_ oo :as v]]
  (let [oo (simp/simplify* oo)]
    (try 
      (match [(vec (flatten oo))]
             ;; PI-angle
             [['+ '%pi '* -1 x]] (cond
                                  (= (trig-kind v) :sin) (list 'sin x)
                                  (= (trig-kind v) :cos) (list '* -1 (list 'cos x))
                                  (= (trig-kind v) :tan) (list '* -1 (list 'tan x))
                                  (= (trig-kind v) :cot) (list '* -1 (list 'cot x))
                                  :else v)
             ;; PI + angle
             [['+ '%pi x]] (cond
                            (= (trig-kind v) :sin) (list '* -1 (list 'sin x))
                            (= (trig-kind v) :cos) (list '* -1 (list 'cos x))
                            (= (trig-kind v) :tan) (list 'tan x)
                            (= (trig-kind v) :cot) (list 'cot x)
                            :else v)
             ;; 2PI - angle = -angle
             [['+ '* 2 '%pi '* -1 x]] (cond 
                                       (= (trig-kind v) :sin) (list '* -1 (list 'sin x))
                                       (= (trig-kind v) :cos) (list 'cos x)
                                       (= (trig-kind v) :tan) (list '* -1 (list 'tan x))
                                       (= (trig-kind v) :cot) (list '* -1 (list 'cot x))
                                       :else v)
             ;; 2*k*PI + angle
             [['+ '* y '%pi x]] (cond 
                                 (and (= (mod y 2) 0) (= (trig-kind v) :sin)) (list 'sin x)
                                 (and (= (mod y 2) 0) (= (trig-kind v) :cos)) (list 'cos x)
                                 (and (= (mod y 2) 0) (= (trig-kind v) :tan)) (list 'tan x)
                                 (and (= (mod y 2) 0) (= (trig-kind v) :cot)) (list 'cot x)
                                 :else v)
             ;; -angle
             [['* -1 x]] (cond 
                          (= (trig-kind v) :sin) (list '* -1 (list 'sin x))
                          (= (trig-kind v) :cos) (list 'cos x)
                          (= (trig-kind v) :tan) (list '* -1 (list 'tan x))
                          (= (trig-kind v) :cot) (list '* -1 (list 'cot x))
                          :else v)
             [_] v)
      (catch Exception e v))))

;;; special angles
;;; TODO: need a reverse rule for this
(defn tr4 [[_ oo :as v]]
  (let [oo (simp/simplify* oo)]
    (if (= oo 0)
          (cond
           (= (trig-kind v) :sin) 0
           (= (trig-kind v) :cos) 1
           (= (trig-kind v) :tan) 0)
          (try 
            (match [(vec (flatten oo))]
                   [['* '/ 1 6 %pi]] (cond
                                        (= (trig-kind v) :sin) '(/ 1 2)
                                        (= (trig-kind v) :cos) (simp/simplify* '(/ (** 3 (/ 1 2)) 2))
                                        (= (trig-kind v) :tan) (simp/simplify* '(/ (** 3 (/ 1 2)) 3))
                                        :else v)
                   [['* '/ 1 4 %pi]] (cond
                                        (= (trig-kind v) :sin) (simp/simplify* '(/ (** 2 (/ 1 2)) 2))
                                        (= (trig-kind v) :cos) (simp/simplify* '(/ (** 2 (/ 1 2)) 2))
                                        (= (trig-kind v) :tan) 1 
                                        :else v)
                   [['* '/ 1 3 %pi]] (cond
                                        (= (trig-kind v) :sin) (simp/simplify* '(/ (** 3 (/ 1 2)) 2))
                                        (= (trig-kind v) :cos) '(/ 1 2)
                                        (= (trig-kind v) :tan) (simp/simplify* '(** 3 (/ 1 2)))
                                        :else v)
                   [['* '/ 1 2 %pi]] (cond
                                        (= (trig-kind v) :sin) 1 
                                        (= (trig-kind v) :cos) 0
                                        :else v)
                   [_] v) 
            (catch Exception e v)))))

;;; sin^2(a) rule

(defn tr5 [op]
  (cond
   (= (simp/kind (simp/simplify* op)) :powop)
   (let [[_ x y] op]
     (if (and (= (trig-kind x) :sin) (= y 2)) 
       (let [[_ x] x]
         (simp/simplify* (list '- 1 (list '** (list 'cos x) 2))))
      op))
   :else op))

;;; cos^2(a) rule
(defn tr6 [op]
  (cond
   (= (simp/kind (simp/simplify* op)) :powop)
   (let [[_ x y] op]
     (if (and (= (trig-kind x) :cos) (= y 2)) 
       (let [[_ x] x]
         (simp/simplify* (list '- 1 (list '** (list 'sin x) 2)))) 
       op))
   :else op))


;;; cos^2(a) rule lowering angle
(defn tr7 [op]
  (cond
   (= (simp/kind (simp/simplify* op)) :powop)
   (let [[_ x y] op]
     (if (and (= (trig-kind x) :cos) (= y 2)) 
       (let [[_ x] x]
         (simp/simplify* (list '/ (list '+ 1 (list 'cos (list '* 2 x))) 2)))
       op))
   :else op))

;;; product to sum or difference
(defn tr8 [op]
  (cond
   (= (simp/kind op) :prodop)
   (if (= (count (rest op)) 2)
     (let [[_ x y] op]
       (match [[(trig-kind x) (trig-kind y)]]
              [[:sin :cos]] (let [[_ x] x
                                  [_ y] y
                                  f (list '+ x y)
                                  s (list '- x y)]
                              (simp/simplify* (list '/ (list '+ (list 'sin f) (list 'sin s)) 2)))
              [[:cos :sin]] (let [[_ x] x
                                  [_ y] y
                                  f (list '+ x y)
                                  s (list '- x y)]
                              (simp/simplify* (list '/ (list '- (list 'sin f) (list 'sin s)) 2)))
              [[:cos :cos]] (let [[_ x] x
                                  [_ y] y
                                  f (list '+ x y)
                                  s (list '- x y)]
                              (simp/simplify* (list '/ (list '+ (list 'cos f) (list 'cos s)) 2)))
              [[:sin :sin]] (let [[_ x] x
                                  [_ y] y
                                  f (list '+ x y)
                                  s (list '- x y)]
                              (simp/simplify* (list '* (list '- (list 'cos s) (list 'cos f)) (list '** 2 -1))))
              [_] op))
     op)
   :else op))

;;; converting sum or diff to product
(defn tr9 [op]
  (cond 
   (and (= (count (rest op)) 2) (= (simp/kind (simp/simplify* op)) :sumop))
   (let [[_ x y] op]
     (cond
      (and (= (trig-kind x) :sin) (= (trig-kind y) :sin))
      (let [[_ x] x
            [_ y] y
            f (list '/ (list '+ x y) 2)
            s (list '/ (list '- x y) 2)]
        (simp/simplify* (list '* 2 (list 'sin f) (list 'cos s))))
      (and (= (trig-kind x) :cos) (= (trig-kind y) :cos))
      (let [[_ x] x
            [_ y] y
            f (list '/ (list '+ x y) 2)
            s (list '/ (list '- x y) 2)]
        (simp/simplify* (list '* 2 (list 'cos f) (list 'cos s))))
      (= (simp/kind x) :prodop)
      (let [[_ t x] x]
        (cond 
         (and (= t -1) (= (trig-kind x) :sin) (= (trig-kind y) :sin))
         (let [[_ x] x
               [_ y] y
               f (list '/ (list '+ x y) 2)
               s (list '/ (list '- x y) 2)]
           (simp/simplify* (list '* 2 (list 'cos f) (list 'sin s))))  
         (and (= t -1) (= (trig-kind x) :cos) (= (trig-kind y) :cos))
         (let [[_ x] x
               [_ y] y
               f (list '/ (list '+ x y) 2)
               s (list '/ (list '- x y) 2)]
           (simp/simplify* (list '* -1 2 (list 'sin f) (list 'sin s))))
         :else op))
      :else op))
   :else op))

;;; sum or diff of angles
(defn tr10 [v]
  (cond 
   (= (trig-kind v) :sin)
   (let [[_ x] v]
     (let [x (simp/simplify* x)]
       (cond
        (= (simp/kind x) :sumop)
        (let [[_ x y :as xx] x]
          (if (util/is-addition? xx)
            (simp/simplify* (list '+ (list '* (list 'sin x) (list 'cos y)) (list '* (list 'cos x) (list 'sin y))))
            (let [[_ _ y] y]
                 (simp/simplify* (list '- (list '* (list 'sin x) (list 'cos y)) (list '* (list 'cos x) (list 'sin y)))))))
        :else v)))
   (= (trig-kind v) :cos)
   (let [[_ x] v]
     (let [x (simp/simplify* x)]
       (cond
        (= (simp/kind x) :sumop)
        (let [[_ x y :as xx] x]
          (if (util/is-addition? xx)
            (simp/simplify* (list '- (list '* (list 'cos x) (list 'cos y)) (list '* (list 'sin x) (list 'sin y))))
            (let [[_ _ y] y] 
              (simp/simplify* (list '+ (list '* (list 'cos x) (list 'cos y)) (list '* (list 'sin x) (list 'sin y)))))))
        :else v)))
   :else v))


;;; double angles
(defn tr11 [op]
  (cond
   (= (trig-kind op) :sin)
   (let [[_ x] op
         x (simp/simplify* x)]
     (cond
      (or (= (simp/kind x) :number) (= (simp/kind x) :fracop)) 
      (let [[xx n d] (if (= (simp/kind x) :number) [x nil nil]
                   [(/ (let [[_ n _] x] n) (let [[_ _ d] x] d)) (let [[_ n _] x] n) (let [[_ _ d] x] d)])]
        (cond
         (and (= (mod xx 2)) (not d) (not n)) (simp/simplify* (list '* 2 (list 'sin (/ xx 2)) (list 'cos (/ xx 2))))
         (and (= (mod xx 2)) (not (= d nil)) (not (= n nil))) 
         (simp/simplify* (list '* 2 (list 'sin (list '/ (/ n 2) d)) (list 'cos (list '/ (/ n 2) d))))
         :else op))
      (= (simp/kind x) :prodop)
      (let [[_ x y] x
            [xx n d] 
            (cond 
             (= (simp/kind x) :number) [x nil nil]
             (= (simp/kind x) :fracop) [(/ (let [[_ n _] x] n) (let [[_ _ d] x] d)) (let [[_ n _] x] n) (let [[_ _ d] x] d)]
             :else [x nil nil])]
        (cond
         (or (= (simp/kind x) :number) (= (simp/kind x) :fracop)) 
         (cond
          (and (= (mod xx 2)) (not d) (not n)) 
          (simp/simplify* (list '* 2 (list 'sin (list '* (/ xx 2) y)) (list 'cos (list '* (/ xx 2) y))))
          (and (= (mod xx 2)) (not (= d nil)) (not (= n nil)))
          (simp/simplify* (list '* 2 (list 'sin (list '* (list '/ (/ n 2) d) y)) (list 'cos (list '* (list '/ (/ n 2) d) y))))
          :else op)
         :else op))
      :else op))
   (= (trig-kind op) :cos)
   (let [[_ x] op
         x (simp/simplify* x)]
     (cond
      (or (= (simp/kind x) :number) (= (simp/kind x) :fracop)) 
      (let [[xx n d] 
            (if (= (simp/kind x) :number) [x nil nil]
                [(/ (let [[_ n _] x] n) (let [[_ _ d] x] d)) (let [[_ n _] x] n) (let [[_ _ d] x] d)])]
        (cond
         (and (= (mod xx 2)) (not d) (not n)) 
         (simp/simplify* (list '- (list '* 2 (list '** (list 'cos (/ x 2)) 2)) 1))
         (and (= (mod xx 2)) (not (= d nil)) (not (= n nil)))
         (simp/simplify* (list '- (list '* 2 (list '** (list 'cos (list '/ (/ n 2) d)) 2)) 1))
         :else op))
      (= (simp/kind x) :prodop)
      (let [[_ x y] x
            [xx n d] 
            (cond 
             (= (simp/kind x) :number) [x nil nil]
             (= (simp/kind x) :fracop) [(/ (let [[_ n _] x] n) (let [[_ _ d] x] d)) (let [[_ n _] x] n) (let [[_ _ d] x] d)]
             :else [x nil nil])]
        (cond
         (or (= (simp/kind x) :number) (= (simp/kind x) :fracop)) 
         (cond
          (and (= (mod xx 2)) (not d) (not n)) 
          (simp/simplify* (list '- (list '* 2 (list '** (list 'cos (list '* (/ xx 2) y)) 2)) 1))
          (and (= (mod xx 2)) (not (= d nil)) (not (= n nil)))
          (simp/simplify* (list '- (list '* 2 (list '** (list 'cos (list '* (list '/ (/ n 2) d) y)) 2)) 1)))
         :else op))
      :else op))
   :else op))

;;; sum or difference of tan
(defn tr12 [v]
  (cond 
   (= (trig-kind v) :tan)
   (let [[_ x] v]
     (let [x (simp/simplify* x)]
       (cond
        (= (simp/kind x) :sumop)
        (let [[_ x y :as xx] x]
          (if (util/is-addition? xx)
            (simp/simplify* (list '/ (list '+ (list 'tan x) (list 'tan y)) (list '- 1 (list '* (list 'tan x) (list 'tan y)))))
            (let [[_ _ y] y]
                (simp/simplify* (list '/ (list '- (list 'tan x) (list 'tan y)) (list '+ 1 (list '* (list 'tan x) (list 'tan y))))))))
        :else v)))
   :else v))


;;; product of tan or cot
(defn tr13 [op]
  (cond
   (= (simp/kind op) :prodop)
   (let [[_ x y] op]
     (cond
      (and (= (trig-kind x) :tan) (= (trig-kind y) :tan))
      (let [[_ x] x [_ y] y]
        (simp/simplify* (list '- 1 (list '* (list '+ (list 'tan x) (list 'tan y)) (list 'cot (list '+ x y))))))
      (and (= (trig-kind x) :cot) (= (trig-kind y) :cot))
      (let [[_ x] x [_ y] y]
        (simp/simplify* (list '+ 1 (list '* (list '+ (list 'cot x) (list 'cot y)) (list 'cot (list '+ x y))))))
      :else op))
   :else op))

;;; All inverse of the above identities go below.


;;; Joel S. Cohen Elementrary CAS trig simplification

(defn trig-substitute [op]
  (cond
   (or (=(simp/kind op) :number)
       (=(simp/kind op) :fracop)
       (=(simp/kind op) :symbol))
   op
   :else (let [u (map trig-substitute op)]
           (cond
            (or (= (trig-kind u) :cot) (= (trig-kind u) :tan)) (tr2 u)  
            (or (= (trig-kind u) :sec) (= (trig-kind u) :csc)) (tr1 u)
            :else u))))


;;; FIXME: A different algorithm for integer multiple angles can be
;;; used.
(defn- expand-trig-rules [A]
  (cond
   (= (simp/kind A) :sumop)  
   (let [f (expand-trig-rules (second A))
         r (expand-trig-rules (third A))
         s (list '+ (list '* (first f) (second r)) (list '* (second f) (first r)))
         c (list '- (list '* (second f) (second r)) (list '* (first f) (first r)))
         ] 
     [s c])
   (= (simp/kind A) :prodop) (if (integer? (second A))
                               ;; then
                               (let [A (reduce #(list '+ % %2) (repeat (second A) (third A)))]
                                 (map expand/expand* (expand-trig-rules A)))
                               ;; else
                               [(list 'sin A) (list 'cos A)])
   :else [(list 'sin A) (list 'cos A)]))

(defn expand-trig [u]
  (cond
   (or (= (simp/kind u) :number) (= (simp/kind u) :fracop) (= (simp/kind u) :symbol)) 
   u
   :else (let [v (map expand-trig u)]
           (cond
            (= (trig-kind v) :sin) (first (expand-trig-rules (fnext v)))
            (= (trig-kind v) :cos) (fnext (expand-trig-rules (fnext v)))
            :else v))))

;;; FIXME: this can be made quicker with sets rather that lists.
(defn separate-sin-cos [u]
  ;; There is a problem in get-prod-operands when there are multiple or single 
  (cond
   (= (simp/kind u) :prodop)
   (let [s (filter #(cond
                     (or (= (trig-kind %) :cos) (= (trig-kind %) :sin)) true
                     (= (simp/kind %) :powop) 
                     (cond
                      (or (= (trig-kind (second %)) :cos) (= (trig-kind (second %)) :sin)) (and (integer? (third %)) (> (third %) 0))
                      :else false)
                     :else false) (do (prn u) (simp/get-prod-operands (rest u))))
         s (if-not (empty? s) (reduce #(list '* % %2) s) 1)
         r (filter #(cond
                     (or (= (trig-kind %) :cos) (= (trig-kind %) :sin)) false
                     (= (simp/kind %) :powop)
                     (cond
                      (or (= (trig-kind (second %)) :cos) (= (trig-kind (second %)) :sin)) (not (and (integer? (third %)) (> (third %) 0)))
                      :else true)
                     :else true) (do (prn u) (simp/get-prod-operands (rest u))))
         r (if-not (empty? r) (reduce #(list '* % %2) 1 r) 1)
         ] [r s])
   (= (simp/kind u) :powop)
   (cond
    (or (= (second u) :cos) (= (second u) :sin))
    (if (and (integer? (third u)) (> (third u) 0)) [1 u] [u 1])
    :else [u 1])
   (or (= (trig-kind u) :cos) (= (trig-kind u) :sin)) [1 u]
   :else [u 1]))

(deftrace contract-trig-product [u]
  (do 
    (if (= (count u) 3)
      ;; then
      (cond
       (= (simp/kind (second u)) :powop)
       (contract-trig-rules (list '* (contract-trig-power (second u)) (third u)))
       (= (simp/kind (third u)) :powop)
       (contract-trig-rules (list '* (second u) (contract-trig-power (third u))))
       :else (tr8 u))
      ;; else
      (let [A (second u)
            B (contract-trig-product (cons '* (nnext u)))]
        (contract-trig-rules (list '* A B))))))

;;; FIXME: The power operation can be done using binomial algorithms to
;;; avoid excessive recursion
(deftrace contract-trig-power [v]
  (if (or (= (simp/kind (second v)) :fracop) (= (simp/kind (second v)) :number) (= (simp/kind (second v)) :symbol))
    v
    (contract-trig-rules (reduce #(list '* % %2) (repeat (third v) (second v))))))

(defn contract-trig-rules [u]
  (let [v (expand/expand-main-op u)]
    (cond
     (= (simp/kind v) :powop) (contract-trig-power v)
     (= (simp/kind v) :prodop)
     (let [[c d] (separate-sin-cos v)]
       (prn "c:" c)
       (prn "d:" d)
       (cond 
        (= d 1) v
        (or (= (trig-kind d) :sin) (= (trig-kind d) :cos)) v
        (= (simp/kind d) :powop) (expand/expand-main-op (list '* c (contract-trig-power d)))
        :else (expand/expand-main-op (list '* c (contract-trig-product (cons '* (simp/get-prod-operands (rest d))))))))
     (= (simp/kind v) :sumop)
     (reduce #(if (or (= (simp/kind %2) :prodop) (= (simp/kind %2) :powop))
                  (list '+ % (contract-trig-rules %2))
                  (list '+ % %2)) 0 (rest v))
     :else v)))

(defn contract-trig [u]
  (cond
   (or (= (simp/kind u) :symbol) (= (simp/kind u) :fracop) (= (simp/kind u) :number))
   u
   :else (let [v (map contract-trig u)]
           (if (or (= (simp/kind v) :powop) (= (simp/kind v) :prodop))
             (contract-trig-rules v)
             v))))

(defn simplify-trig-operands [u]
  (cond
   (= (trig-kind u) :other) (simp/simplify* u)
   :else (list (first u) (simplify-trig-operands (fnext u)))))

(defn expand-trig-operands [u]
  (cond
   (= (trig-kind u) :other) (simp/simplify* (expand/expand* u))
   :else (list (first u) (expand-trig-operands (fnext u)))))

(defn apply-induced-identities [u]
  (cond
   (and (not (= (simp/kind u) :symbol)) (not (= (simp/kind u) :number)) (not (= (simp/kind u) :fracop)) (= (trig-kind u) :other)) 
   (let [[x y z] u] 
     (list x (apply-induced-identities y) (apply-induced-identities z)))
   (and (not (= (simp/kind u) :symbol)) (not (= (simp/kind u) :number)) (not (= (simp/kind u) :fracop))) (tr3 u)
   :else u))

(defn apply-special-identities [u]
  (cond
   (or (= (simp/kind u) :symbol) (= (simp/kind u) :number) (= (simp/kind u) :fracop)) u
   (= (trig-kind u) :other)
   (let [[x y z] u] 
     (list x (apply-special-identities y) (apply-special-identities z)))
   :else (tr4 u)))


(defn trig-simplify* [u]
  (let [
        u (simplify-trig-operands u)
        ;; expand the operands recursively
        _ (prn "u1:" u)
        u (expand-trig-operands u)
        _ (prn "u2:" u)
        ;; apply induced identities
        u (simp/simplify* (apply-induced-identities u))
        _ (prn "u3:" u)
        ;; apply special angles
        u ((comp simp/simplify* apply-special-identities) u)
        _ (prn "u4:" u)
        u ((comp simp/simplify* trig-substitute) u)
        _ (prn "u5:" u)
        u (natural/natural* u)
        _ (prn "u6:" u)
        w (simp/simplify* u)
        _ (prn "w:" w)
        n (expand-trig (natural/numer w))
        _ (prn "expand n:" n)
        n (contract-trig n)
        _ (prn "contract n:" n)
        n (apply-special-identities n)
        _ (prn "n:" n)
        n (apply-induced-identities n)
        _ (prn "n:" n)
        n (simp/simplify* n)
        _ (prn "n:" n)
        d (expand-trig (natural/denom w))
        _ (prn "expand d:" d)
        d (contract-trig d)
        _ (prn "contract d:" d)
        ;; d (simp/simplify* (apply-induced-identities (apply-special-identities (contract-trig (expand-trig (natural/denom w))))))
        d (apply-special-identities d)
        _ (prn "d:" d)
        d (apply-induced-identities d)
        _ (prn "d:" d)
        d (simp/simplify* d)
        _ (prn "d:" d)
        ]
    (if (= d 0) 'UNDEFINED (simp/simplify* (list '* (list '** d -1) n)))))


(defmacro trig-simplify [& args]
  `(map (comp simp/simplify* trig-simplify* simp/simplify*) '(~@args)))
