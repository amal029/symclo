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
         [:sec] (simp/simplify* (list '/ 1 (list 'cos oo)))
         [:csc] (simp/simplify* (list '/ 1 (list 'sin oo)))
         [_] v))

(defn tr2 [[_ oo :as v]]
  (match [(trig-kind v)]
         [:tan] (simp/simplify* (list '/ (list 'sin oo) (list 'cos oo)))
         [:cot] (simp/simplify* (list '/ (list 'cos oo) (list 'sin oo)))
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
   (= (simp/kind (simp/simplify* op)) :prodop)
   (if (= (count (rest op)) 2)
     (let [[_ x y] (simp/simplify* op)]
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
                              (simp/simplify* (list '/ (list '- (list 'cos f) (list 'cos s)) (list '* -1 2))))
              [_] op))
     op)
   (= (simp/kind (simp/simplify* op)) :powop)
   (let [[_ x y] (simp/simplify* op)]
     (if (= y 2)
       (match [(trig-kind x)]
              [:cos] (let [[_ x] x
                           f (list '+ x x)
                           s (list '- x x)]
                       (simp/simplify* (list '/ (list '+ 1 (list 'cos f)) 2)))
              [:sin] (let [[_ x] x
                           f (list '+ x x)]
                       (simp/simplify* (list '/ (list '- (list 'cos f) 1) (list '* -1 2))))
              [_] op)
       op))
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
