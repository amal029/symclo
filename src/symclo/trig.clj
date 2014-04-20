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

(defn- trig-kind [op]
  (cond 
   (= (first op) 'sin) :sin
   (= (first op) 'cos) :cos
   (= (first op) 'tan) :tan
   (= (first op) 'cot) :cot
   (= (first op) 'sec) :sec
   (= (first op) 'csc) :csc
   :else :other))

;;; The trignometric identities from text books trig-functions can only
;;; ever have 1 operand

(defn- tr1 [[_ oo :as v]]
  (match [(trig-kind v)]
         [:sec] (list '** (list 'cos oo) -1)
         [:csc] (list '** (list 'sin oo) -1)
         [_] v))

(defn- tr2 [[_ oo :as v]]
  (match [(trig-kind v)]
         [:tan] (list ('/ (list 'sin oo) (list 'cos oo)))
         [:cot] (list ('/ (list 'cos oo) (list 'sin oo)))
         [_] v))

(defn- tr3 [[_ oo :as v]]
  (let [oo (simp/simplify oo)]
    (match [oo]
           ;; -angle
           [['* -1 x]] (cond 
                        (= (trig-kind v) :sin) (list '* -1 (list 'sin x))
                        (= (trig-kind v) :cos) (list 'cos x)
                        (= (trig-kind v) :tan) (list '* -1 (list 'tan x))
                        (= (trig-kind v) :cot) (list '* -1 (list 'cot x))
                        :else v)
           ;; PI-angle
           [['+ '%pi ['* -1 x]]] (cond
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
           [['+ ['* 2 '%pi] ['* -1 x]]] (cond 
                                         (= (trig-kind v) :sin) (list '* -1 (list 'sin x))
                                         (= (trig-kind v) :cos) (list 'cos x)
                                         (= (trig-kind v) :tan) (list '* -1 (list 'tan x))
                                         (= (trig-kind v) :cot) (list '* -1 (list 'cot x))
                                         :else v)
           ;; 2*k*PI + angle
           [['+ ['* y '%pi] x]] (cond 
                                         (and (= (mod y 2) 0) (= (trig-kind v) :sin)) (list 'sin x)
                                         (and (= (mod y 2) 0) (= (trig-kind v) :cos)) (list 'cos x)
                                         (and (= (mod y 2) 0) (= (trig-kind v) :tan)) (list 'tan x)
                                         (and (= (mod y 2) 0) (= (trig-kind v) :cot)) (list 'cot x)
                                         :else v)
           [_] v)))

;;; special angles
;;; TODO: need a reverse rule for this
(defn- tr4 [[_ oo :as v]]
  (let [oo (simp/simplify oo)]
    (match [oo]
           [0] (cond
                (= (trig-kind v) :sin) 0
                (= (trig-kind v) :cos) 1
                (= (trig-kind v) :tan) 0)
           [['* ['** 6 -1] %pi]] (cond
                                  (= (trig-kind v) :sin) '(/ 1 2)
                                  (= (trig-kind v) :cos) (simp/simplify (/ (** 3 (/ 1 2)) 2))
                                  (= (trig-kind v) :tan) (simp/simplify (/ (** 3 (/ 1 2)) 3))
                                  :else v)
           [['* ['** 4 -1] %pi]] (cond
                                  (= (trig-kind v) :sin) (simp/simplify (/ (** 2 (/ 1 2)) 2))
                                  (= (trig-kind v) :cos) (simp/simplify (/ (** 2 (/ 1 2)) 2))
                                  (= (trig-kind v) :tan) 1 
                                  :else v)
           [['* ['** 3 -1] %pi]] (cond
                                  (= (trig-kind v) :sin) (simp/simplify (/ (** 3 (/ 1 2)) 2))
                                  (= (trig-kind v) :cos) '(/ 1 2)
                                  (= (trig-kind v) :tan) (simp/simplify (** 3 (/ 1 2)))
                                  :else v)
           [['* ['** 2 -1] %pi]] (cond
                                  (= (trig-kind v) :sin) 1 
                                  (= (trig-kind v) :cos) 0
                                  :else v)
           [_] v)))

;;; sin^2(a) rule

(defn- tr5 [op]
  (cond
   (= (simp/kind (simp/simplify* op)) :powop)
   (let [[_ x y] op]
     (if (and (= (trig-kind x) :sin) (= y 2)) 
       (let [[_ x] x]
         (simp/simplify* (list '- 1 (list '** (list 'cos x) 2)))
         op
         )))
   :else op))

;;; cos^2(a) rule
(defn- tr6 [op]
  (cond
   (= (simp/kind (simp/simplify* op)) :powop)
   (let [[_ x y] op]
     (if (and (= (trig-kind x) :cos) (= y 2)) 
       (let [[_ x] x]
         (simp/simplify* (list '- 1 (list '** (list 'sin x) 2)))
         op
         )))
   :else op))


;;; cos^2(a) rule lowering angle
(defn- tr6 [op]
  (cond
   (= (simp/kind (simp/simplify* op)) :powop)
   (let [[_ x y] op]
     (if (and (= (trig-kind x) :cos) (= y 2)) 
       (let [[_ x] x]
         (simp/simplify* (list '/ (list '+ 1 (list 'cos (list '* 2 x))) 2))
         op
         )))
   :else op))

;;; product to sum or difference
(defn- tr8 [op]
  (cond
   (= (simp/kind (simp/simplify* op)) :prodop)
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
                              (simp/simplify* (list '/ (list '- (list 'sin f) (list 'sin s)) (list * -1 2))))
              [_] op))
     op)
   :else op))

;;; converting sum or diff to product
(defn- tr9 [op]
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
      (= (simp/kind y) :prodop)
      (let [[_ t y] y]
        (cond 
         (and (= t -1) (= (trig-kind y) :sin) (= (trig-kind x) :sin))
         (let [[_ x] x
               [_ y] y
               f (list '/ (list '+ x y) 2)
               s (list '/ (list '- x y) 2)]
           (simp/simplify* (list '* 2 (list 'cos f) (list 'sin s))))  
         (and (= t -1) (= (trig-kind y) :cos) (= (trig-kind x) :cos))
         (let [[_ x] x
               [_ y] y
               f (list '/ (list '+ x y) 2)
               s (list '/ (list '- x y) 2)]
           (simp/simplify* (list '* -1 2 (list 'sin f) (list 'sin s))))
         :else op))
      :else op))
   :else op))

;;; TODO sum or diff of angles
;;; TODO We need a reverse for this
(defn- tr10 [op]
  op)


;;; double angles
(defn- tr11 [op]
  (cond
   (= (trig-kind op) :sin)
   (let [[_ x] op
         x (simp/simplify* x)]
     (cond
      (or (= (simp/kind x) :number) (= (simp/simplify* x) :fracop)) 
      (let [x (simp/simplify-rne x)]
        (cond
         (= (mod x 2)) (simp/simplify* (list '* 2 (list 'sin (/ x 2)) (list 'cos (/ x 2))))
         :else op))
      (= (simp/kind x) :prodop) 
      (let [[_ x y] x]
        (cond
         (= (mod x 2)) (simp/simplify* (list '* 2 (list 'sin (list '* (/ x 2) y)) (list 'cos (list '* (/ x 2) y))))
         :else op))
      :else op))
   (= (trig-kind op) :cos)
   (let [[_ x] op
         x (simp/simplify* x)]
     (cond
      (or (= (simp/kind x) :number) (= (simp/simplify* x) :fracop)) 
      (let [x (simp/simplify-rne x)]
        (cond
         (= (mod x 2)) (simp/simplify* (list '- (list '* 2 (list '** (list 'cos (/ x 2)) 2)) 1))
         :else op))
      (= (simp/kind x) :prodop) 
      (let [[_ x y] x]
        (cond
         (= (mod x 2)) (simp/simplify* (list '- (list '* 2 (list '** (list 'cos (list '* (/ x 2) y)) 2)) 1))
         :else op))
      :else op))
   :else op))

;;; sum or difference of tan
;;; TODO
(defn- tr12 [[ _ x :as v]]
  (cond
   (= (trig-kind v) :tan)
   (cond
    (= (simp/kind (simp/simplify* x)) :sumop)
    (let [[_ x y] x]
      (cond
       (not (or (= (simp/kind x) :prodop) (= (simp/kind y) :prodop)))
       (simp/simplify* (list '/ (list '+ (list 'tan x) (list 'tan y)) (list '- 1 (list '* (list 'tan x) (list 'tan y)))))
       (= (simp/kind x) :prodop)
       (let [[_ x x]])
       :else v
       ))
    :else v)
   :else v))
