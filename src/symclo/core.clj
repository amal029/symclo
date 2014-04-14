(ns symclo.core
  (:gen-class))
(use '[clojure.core.match :only (match)])


;;; Euclid's gcd algorithm
(defn integer_gcd 
  "(gcd a b) computes the greatest common divisor of a and b."
  [a b]
  (if (zero? b) a
      (recur b (mod a b))))

;;; The type of the operator
(defn- kind-op [op]
  (cond 
   (= (first op) '/) :fracop
   (= (first op) '*) :prodop
   (= (first op) '**) :powop
   (= (first op) '+) :sumop
   (= (first op) '-) :diffop
   (= (first op) '!) :factop
   (= (first op) '\\) :quotop
   :else (throw "Unknown operator: " op)))

;;; The type of the number or operator?
(defn- kind [u]
  (cond 
   (integer? u) :number
   (symbol? u) :symbol
   (and (not (integer? u)) (number? u)) (throw "fractions should only have integer numerators and denominators: " u)
   :else (kind-op u)))

(defn- simplify-rational-number [u]
  (match [(kind u)]
         [:number] u
         [:fracop] (let [[_ n d] u]
                     (cond
                      (= (mod n d) 0) (list '\\ n d)  ; giving the quotient back as a symbol
                      (> d 0) (list '/ (list '\\ n (integer_gcd n d)) (list '\\ d (integer_gcd n d)))
                      (< d 0) (list '/ (list '\\ (list '* -1 n) (integer_gcd n d)) (list '\\ (list '* -1 d) (integer_gcd n d)))
                      :else 'UNDEFINED))))


;;; forward declarations for multually recursive functions
(declare simplify-power)
(declare simplify-sum)
(declare simplify-product)
(declare simplify-factorial)
(declare simplify-function)
(declare simplify-quotient)
(declare simplify-diff)

;;; simplify-power
(defn- simplify-power [u]
  )

;;; simplify-sum
(defn- simplify-sum [u]
  u )

;;; simplify-diff
;;; Can be unary or binary
;;; For now only binary is supported
;;; write unary as multiplication with -1
(defn- simplify-diff [op x y]
  (simplify-sum (list '+ x (list '* -1 y))))
;;; simplify-quotient
;;; This can only ever have two operand!
(defn- simplify-quotient [[_ n d]]
  (simplify-product (list '* n (list '** d -1))))

;;; simplify-factorial
(defn- factorial [op]
  (cond 
   (= op 1) op
   :else (* op (factorial (- op 1)))))

(defn- simplify-factorial [[_ op]]
   (if (integer? op) (factorial op) (list '! op)))

;;; simplify-function 
;; FIXME: can add trignometric and other function
;;; here
(defn- simplify-function [u]
  u)

;;; simplify-product
(defn- simplify-product [u]
  u)


;;; The main automatic simplification function
(defn simplify* [u]
  (match [(kind u)]
         [:number] u
         [:symbol] u
         [:fracop] (simplify-rational-number u)
         [_] (let [v (map-indexed #(if-not (= % 0) (simplify* %2) %2) u)]
               (match [(kind v)]
                      [:powop] (simplify-power v)
                      [:prodop] (simplify-product v)
                      [:sumop] (simplify-sum v)
                      [:quotop] (simplify-quotient v)
                      [:diffop] (simplify-diff v)
                      [:factop] (simplify-factorial v)
                      [_] (simplify-function v)))))

(defn -main [& args]
  "I don't do a whole lot."
  (println args "Hello, World!"))


