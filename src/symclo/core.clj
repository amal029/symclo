(ns symclo.core
  (:gen-class))
(use '[clojure.core.match :only (match)])
(use 'clojure.math.numeric-tower)


;;; Euclid's gcd algorithm
(defn integer_gcd 
  "(gcd a b) computes the greatest common divisor of a and b."
  [a b]
  (if (zero? b) a
      (recur b (mod a b))))

;;; The type of the operator
(defn- kind-op [op]
  (cond 
   (= (first op) '*) :prodop
   (= (first op) '**) :powop
   (= (first op) '+) :sumop
   (= (first op) '-) :diffop
   (= (first op) '!) :factop
   :else (if (and (= (first op) '/) (= (count (rest op)) 2)) 
           (let [[_ x y] op] (if (and (integer? x) (integer? y)) :fracop :quotop))
           (throw (Throwable. (str "Don't know the kind of operation: " op))))))

;;; The type of the number or operator?
(defn- kind [u]
  (cond 
   (integer? u) :number
   (symbol? u) :symbol
   (and (not (integer? u)) (number? u)) (throw (Throwable. (str "fractions should only have integer numerators and denominators: " u)))
   :else (kind-op u)))

(defn- simplify-rational-number [u]
  (match [(kind u)]
         [:number] u
         [:fracop] (let [[_ n d] u]
                     (cond
                      (= (mod n d) 0) (/ n d)  ; giving the quotient back as a symbol
                      (> d 0) (list '/ (/ n (integer_gcd n d)) (/ d (integer_gcd n d)))
                      (< d 0) (list '/ (/ (*' -1 n) (integer_gcd n d)) (/ (*' -1 d) (integer_gcd n d)))
                      :else 'UNDEFINED))))

;;; numerator function
(defn- numerator-function [x]
  (match [(kind x)]
         [:number] x
         [:fracop] (let [[_ n _] x] n)
         [_] (throw (Throwable. (str "numerator-function: only integers and fractions have numberator: " x)))))

;;; denominator function
(defn- denominator-function [x]
  (match [(kind x)]
         [:number] 1
         [:fracop] (let [[_ _ d] x] d)
         [_] (throw (Throwable. (str "numerator-function: only integers and fractions have numerators: " x)))))

;;; All evaluation functions use arbitary precision arithmetic

;;; evaluate quotient
(defn- evaluate-quotient [v w]
  (cond 
   (or (= v 'UNDEFINED) (= w 'UNDEFINED)) 'UNDEFINED
   :else (if-not (= (numerator-function w) 0) (list '/ (*' (numerator-function v) (denominator-function w)) 
                                                    (*' (numerator-function w) (denominator-function v)))
                 'UNDEFINED)))

;;; evaluation functions These are not needed, because clojure natively
;;; supports these functions on ratios, but I am just putting them in so
;;; that no problem happen later on
(defn- evaluate-sum [v w]
  (cond 
   (or (= v 'UNDEFINED) (= w 'UNDEFINED)) 'UNDEFINED
   :else (if-not (or (= (denominator-function v) 0) (= (denominator-function w) 0))
           (list '/ (+' (*' (numerator-function v) (denominator-function w)) (*' (numerator-function w) (denominator-function v))) 
                 (*' (denominator-function v) (denominator-function w)))
           'UNDEFINED)))

(defn- evaluate-product [v w]
  (cond 
   (or (= v 'UNDEFINED) (= w 'UNDEFINED)) 'UNDEFINED
   :else (if-not (or (= (denominator-function v) 0) (= (denominator-function w) 0))
           (list '/ (*' (numerator-function v) (numerator-function w)) (*' (denominator-function v) (denominator-function w)))
           'UNDEFINED)))

(defn- evaluate-diff [v w]
  (cond
   (or (= v 'UNDEFINED) (= w 'UNDEFINED)) 'UNDEFINED
   :else (if-not (or (= (denominator-function v) 0) (= (denominator-function w) 0))
           (list '/ (-' (*' (numerator-function v) (denominator-function w)) (*' (numerator-function w) (denominator-function v))) 
                 (*' (denominator-function v) (denominator-function w)))
           'UNDEFINED)))

(defn- evaluate-power [v n] 
  (cond 
   (or (= v 'UNDEFINED) (= n 'UNDEFINED)) 'UNDEFINED
   :else (if-not (= (numerator-function v) 0) 
           (cond
            (> n 0) (evaluate-product (evaluate-power v (- n 1)) v)
            (= n 0) 1
            ;; this seems wrong!
            (= n -1) (evaluate-quotient 1 v)
            (< n -1) (evaluate-power (evaluate-quotient 1 v) (*' -1 n)))
           ;; This is the else part
           (cond
            (>= n 1) 0
            (<= n 0) 'UNDEFINED))))

;;; the recursive simplify rne function
(defn- simplify-rne-rec [u]
  (match [(kind u)]
         [:number] u
         [:fracop] (let [[_ n d] u] (if (= d 0) 'UNDEFINED u))
         [:sumop] (cond
                   (= (count (rest u)) 1) (let [[_ x] u] (simplify-rne-rec x))
                   (= (count (rest u)) 2) (let [[_ x y] u] (evaluate-sum (simplify-rne-rec x) (simplify-rne-rec y)))
                   :else (throw (Throwable. (str "simplify-rne-rec operator with more than 2 operands: " u))))
         [:diffop] (cond
                    (= (count (rest u)) 1) (let [[_ x] u] (evaluate-product -1 (simplify-rne-rec x)))
                    (= (count (rest u)) 2) (let [[_ x y] u] (evaluate-diff (simplify-rne-rec x) (simplify-rne-rec y)))
                   :else (throw (Throwable. (str "simplify-rne-rec operator with more than 2 operands: " u))))
         [:prodop] (cond
                    (= (count (rest u)) 2) (let [[_ x y] u] (evaluate-product (simplify-rne-rec x) (simplify-rne-rec y)))
                   :else (throw (Throwable. (str "simplify-rne-rec operator with more than 2 operands: " u))))
         [:quotop] (cond
                    (= (count (rest u)) 2) (let [[_ x y] u] (evaluate-quotient (simplify-rne-rec x) (simplify-rne-rec y)))
                    :else (throw (Throwable. (str "simplify-rne-rec operator with more than 2 operands: " u))))
         [:powop] (let [[_ x y] u] (evaluate-power (simplify-rne-rec x) (simplify-rne-rec y)))))

;;; The SIMPLIFY_RNE function
(defn- simplify-rne [u]
  (let [v (simplify-rne-rec u)]
    (simplify-rational-number v)))

;;; forward declarations for mutually recursive functions
(declare simplify-power)
(declare simplify-sum)
(declare simplify-product)
(declare simplify-factorial)
(declare simplify-function)
(declare simplify-quotient)
(declare simplify-diff)

;;; simplify-integer-power
(defn- simplify-integer-power [v n] 
  (cond
   (or (= (kind v) :number) (= (kind v) :fracop)) (simplify-rne (list '** v n))
   (= n 0) 1
   (= n 1) v
   (= (kind v) :powop) (let [[_ r s] v
                             p (simplify-product (list '* s n))]
                         (if (integer? p) (simplify-integer-power r p)
                             (list '** r p)))
   (= (kind v) :prodop) (simplify-product (map-indexed #(if (= % 0) '* (simplify-integer-power %2 n))))
   :else (list '** v n)))

;;; simplify-power
(defn- simplify-power [[op v w]]
  (cond
   (or (= v 'UNDEFINED) (= w 'UNDEFINED)) 'UNDEFINED
   (= v 0) (cond 
            (or (= (kind w) :number) (= (kind w):fracop)) 0
            :else 'UNDEFINED)
   (= v 1) 1
   (= (kind w) :number) (simplify-integer-power (list op v w))
   :else (list op v w)))

;;; simplify-sum
;;; TODO
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

;;; the base
(defn- base [x]
  (match [(kind x)]
         [:symbol] x
         [:prodop] x
         [:sumop] x
         [:factop] x
         [:powop] (let [[_ b _] x] b)
         [:number] 'UNDEFINED
         [:fracop] 'UNDEFINED))

(defn- exponent [x]
  (match [(kind x)]
         [:symbol] 1
         [:prodop] 1
         [:sumop] 1
         [:factop] 1
         [:powop] (let [[_ _ e] x] e)
         [:number] 'UNDEFINED
         [:fracop] 'UNDEFINED))

;;; the lexicographical ordering
;;; used for canonicalization
;;; TODO
(defn- lex-order-less-than [u v]
  (cond
   (and (or (= (kind u) :number) (= (kind u) :fracop))
        (or (= (kind v) :number) (= (kind v) :fracop))) 
   (< u v)
   (and (= (kind u) :symbol) (= (kind v) :symbol)) (let [res (sort (list u v))]
                                                     (cond 
                                                      (= (first res) u) true
                                                      :else false))
   (or (and (= (kind u) :sumop) (= (kind v) :sumop))
       (and (= (kind u) :prodop) (= (kind v) :prodop))) (let [uops (rest u)
                                                              vops (rest v)]
                                                          (cond
                                                           (not (= (last uops) (last vops))) (lex-order-less-than (last uops) (last vops))
                                                           ;; Rule O-3.2
                                                           )) 
   ))

(declare simplify-product-rec)

;;; merge-products
(defn- merge-products [p q] 
  (cond
   (= (count q) 0) p
   (= (count p) 0) q
   :else (let [p1 (first p)
               q1 (first q)
               h (simplify-product-rec)]
           (cond
            (= (count h) 0) (merge-products (rest p) (rest q))
            (= (count h) 1) (list* (first h) (merge-products (rest p) (rest q)))
            (= (count h) 2) (cond
                             (= (first h) p1) (list* p1 (merge-products (rest p) q))
                             (= (first h) q1) (list* q1 (merge-products p (rest q))))))))

;;; The recursive product simplification function
(defn- simplify-product-rec [op]
  (cond
   (and (= (count op) 2) (let [[x y] op] (not (and (= (kind x) :prodop) (= (kind y) :prodop))))) 
   (let [[x y] op]
     (cond
      (and (or (= (kind x) :number) (= (kind x) :fracop)) 
           (or (= (kind y) :number) (= (kind y) :fracop)))
      (let [p (simplify-rne (list '* x y))]
        (if (= p 1) '() (list p)))
      (= x 1) (list y)
      (= y 1) (list x)
      (= (base x) (base y)) (let [s (simplify-sum (list '+ (exponent x) (exponent y)))
                                  p (simplify-power (list '* (base x) s))]
                              (cond 
                               (= p 1) '()
                               (not (= p 1)) (list p)))
      ;; lex-less-than is the ordering constraint for the cannoican normal form
      (lex-order-less-than y x) (list y x)
      :else op)) 
   ;; This is the else part and SPRDEC-2
   (and (= (count op) 2) (let [[x y] op] (or (= (kind x) :prodop) (= (kind y) :prodop))))
   (let [[x y] op]
     (cond
      (and (= (kind x) :prodop) (= (kind y) :prodop)) (merge-products (rest x) (rest y))
      (= (kind x) :prodop) (merge-products (rest x) (list y))
      (= (kind y) :prodop) (merge-products (list x) (rest y))))
   (> (count op) 2) (let [w (simplify-product-rec (rest op))] 
                      (cond
                       (= (kind (first op)) :prodop) (merge-products (rest (first op)) w)
                       :else (merge-products (list (first op)) w)))))

;;; simplify-product
(defn- simplify-product [u]
  (cond
   (some #(= 'UNDEFINED %) u) 'UNDEFINED
   (some #(= 0 %) u) 0
   (= (count u) 1) u
   :else (let [v (simplify-product-rec u)]
           (cond
            (= (count v) 1) v
            (= (count v) 0) 1
            :else (reduce #('* % %2) v)))))


;;; The main automatic simplification function
(defn simplify* [u]
  (match [(kind u)]
         [:number] u
         [:symbol] u
         [:fracop] (simplify-rational-number u)
         [_] (let [v (map-indexed #(if-not (= % 0) (simplify* %2) %2) u)]
               (match [(kind v)]
                      [:powop] (simplify-power v)
                      [:prodop] (simplify-product (rest v))
                      [:sumop] (simplify-sum v)
                      [:quotop] (simplify-quotient v)
                      [:diffop] (simplify-diff v)
                      [:factop] (simplify-factorial v)
                      [_] (simplify-function v)))))

(defn -main [& args]
  "I don't do a whole lot."
  (println args "Hello, World!"))


