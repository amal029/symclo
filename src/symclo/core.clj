(ns symclo.core
  (:gen-class))
(use '[clojure.core.match :only (match)])
(use 'clojure.math.numeric-tower)

;;; forward declarations for mutually recursive functions
(declare simplify-power)
(declare simplify-sum)
(declare simplify-product)
(declare simplify-factorial)
(declare simplify-function)
(declare simplify-quotient)
(declare simplify-diff)
(declare simplify-product-rec)
(declare simplify-sum-rec)

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
   (and (= (first op) '/) (= (count (rest op)) 2)) (let [[_ x y] op] (if (and (integer? x) (integer? y)) :fracop :quotop))
   :else (throw (Throwable. (str op " is type: " (type op) "\n")))))

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

;;; simplify-integer-power
(defn- simplify-integer-power [[op v n]] 
  (cond
   (or (= (kind v) :number) (= (kind v) :fracop)) (simplify-rne (list op v n))
   (= n 0) 1
   (= n 1) v
   (= (kind v) :powop) (let [[_ r s] v
                             p (simplify-product (list s n))]
                         (if (integer? p) (simplify-integer-power (list op r p))
                             (list op r p)))
   (= (kind v) :prodop) (simplify-product (map-indexed #(if (= % 0) (simplify-integer-power (list op %2 n))) v))
   :else (list op v n)))

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

;;; simplify-diff
;;; Can be unary or binary
;;; For now only binary is supported
;;; write unary as multiplication with -1
(defn- simplify-diff [op]
  (let [res (list* (first op) (map #(list '* -1 %) (rest op)))]
    (simplify-sum res)))
;;; simplify-quotient
;;; This can only ever have two operand!
(defn- simplify-quotient [[_ n d]]
  (simplify-product (list n (list '** d -1))))

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
(defn- algebra-compare [u v]
  (cond
   (and (or (= (kind u) :number) (= (kind u) :fracop))
        (or (= (kind v) :number) (= (kind v) :fracop))) 
   (< u v)
   (and (= (kind u) :symbol) (= (kind v) :symbol)) (let [res (sort (list u v))]
                                                     (cond 
                                                      (= (first res) u) true
                                                      :else false))
   (or (and (= (kind u) :sumop) (= (kind v) :sumop))
       (and (= (kind u) :prodop) (= (kind v) :prodop))) 
   (let [uops (rest u)
         vops (rest v)]
     (cond
      (not (= (last uops) (last vops))) (algebra-compare (last uops) (last vops))
      (reduce #(or % %2) (map #(if-not (= % %2) (algebra-compare % %2) false) 
                              (reverse uops) (reverse vops))) true
                              :else (< (count uops) (count vops))))
   (and (= (kind u) :powop) (= (kind v) :powop)) (cond
                                                  (not (= (base u) (base v))) (algebra-compare (base u) (base v))
                                                  :else (algebra-compare (exponent u) (exponent v)))
   (and (= (kind u) :factop) (= (kind v) :factop)) (algebra-compare (last u) (last v))
   (and (= (kind u) :function) (= (kind v) :function)) (cond
                                                        (not (= (first u) (first v))) (algebra-compare (first u) (first v))
                                                        (= (kind u) (kind v)) (cond
                                                                               (not (= (first (rest u)) (first (rest v))))
                                                                               (algebra-compare (first (rest u)) (first (rest v)))
                                                                               (reduce #(or % %2) (map #(if-not (= % %2) (algebra-compare % %2) false) 
                                                                                                       (rest u) (reverse v))) true
                                                                                                       :else (< (count (rest u)) (count (rest v)))))
   ;; O-7
   (and (or (= (kind u) :number) (= (kind u) :fracop)) 
        (and (not (= (kind v) :number)) (not (= (kind v) :fracop)))) 
   true
   ;; O-8
   (and (= (kind u) :prodop) (or (= (kind v) :powop) (= (kind v) :sumop)
                                 (= (kind v) :factop) (= (kind v) :function)
                                 (= (kind v) :symbol))) 
   (algebra-compare u (list '* 1 v))
   ;; O-9
   (and (= (kind u) :powop) (or (= (kind v) :sumop)
                                (= (kind v) :factop) (= (kind v) :function)
                                (= (kind v) :symbol)))
   (algebra-compare u (list '** v 1))
   ;; O-10
   (and (= (kind u) :sumop) (or (= (kind v) :factop) (= (kind v) :function)
                                (= (kind v) :symbol)))
   (algebra-compare u (list '+ 0 v))
   ;; O-11
   (and (= (kind u) :factop) (or (= (kind v) :function) (= (kind v) :symbol)))
   (cond
    (= (last u) v) false
    :else (algebra-compare u (list '! v)))
   ;; O-12
   (and (= (kind u) :function) (= (kind v) :symbol))
   (cond
    (= (first u) v) false
    :else (algebra-compare (first u) v))
   ;; These are the complimentary rules
   ;; O-7 complimentary
   (and (or (= (kind v) :number) (= (kind v) :fracop)) 
        (and (not (= (kind u) :number)) (not (= (kind u) :fracop)))) 
   false
   ;; O-8 compliment
   (and (= (kind v) :prodop) (or (= (kind u) :powop) (= (kind u) :sumop)
                                 (= (kind u) :factop) (= (kind u) :function)
                                 (= (kind u) :symbol))) 
   (algebra-compare (list '* 1 u) v)
   ;; O-9 compliment
   (and (= (kind v) :powop) (or (= (kind u) :sumop)
                                (= (kind u) :factop) (= (kind u) :function)
                                (= (kind u) :symbol)))
   (algebra-compare (list '** u 1) v)
   ;; O-10 compliment
   (and (= (kind v) :sumop) (or (= (kind u) :factop) (= (kind u) :function)
                                (= (kind u) :symbol)))
   (algebra-compare (list '+ 0 u) v)
   ;; O-11 compliment
   (and (= (kind v) :factop) (or (= (kind u) :function) (= (kind u) :symbol)))
   (cond
    (= (last v) u) false
    :else (algebra-compare (list '! u) v))
   ;; O-12 compliment
   (and (= (kind v) :function) (= (kind u) :symbol))
   (cond
    (= (first v) u) false
    :else (algebra-compare u (first v)))
   :else (throw (Throwable. (str "Do not know how to compare: " u " " v)))))


;;; merge-products
(defn- merge-products [p q] 
  (cond
   (= (count q) 0) p
   (= (count p) 0) q
   :else (let [p1 (first p)
               q1 (first q)
               h (simplify-product-rec (list p1 q1))]
           (cond
            (= (count h) 0) (merge-products (rest p) (rest q))
            (= (count h) 1) (list* (first h) (merge-products (rest p) (rest q)))
            (= (count h) 2) (cond
                             (= (first h) p1) (list* p1 (merge-products (rest p) q))
                             (= (first h) q1) (list* q1 (merge-products p (rest q))))))))

;;; merge-sums
(defn- merge-sums [p q] 
  (cond
   (= (count q) 0) p
   (= (count p) 0) q
   :else (let [p1 (first p)
               q1 (first q)
               h (simplify-sum-rec (list p1 q1))]
           (cond
            (= (count h) 0) (merge-sums (rest p) (rest q))
            (= (count h) 1) (list* (first h) (merge-sums (rest p) (rest q)))
            (= (count h) 2) (cond
                             (= (first h) p1) (list* p1 (merge-sums (rest p) q))
                             (= (first h) q1) (list* q1 (merge-sums p (rest q))))))))

;;; The recursive product simplification function
(defn- simplify-product-rec [op]
  (cond
   (and (= (count op) 2) (let [[x y] op] (not (or (= (kind x) :prodop) (= (kind y) :prodop))))) 
   (let [[x y] op]
     (cond
      (and (or (= (kind x) :number) (= (kind x) :fracop)) 
           (or (= (kind y) :number) (= (kind y) :fracop)))
      (let [p (simplify-rne (list '* x y))]
        (if (= p 1) '() (list p)))
      (= x 1) (list y)
      (= y 1) (list x)
      (= (base x) (base y)) (let [s (simplify-sum (list (exponent x) (exponent y)))
                                  p (simplify-power (list '** (base x) s))]
                              (cond 
                               (= p 1) '()
                               (not (= p 1)) (list p)))
      ;; lex-less-than is the ordering constraint for the cannoican normal form
      (algebra-compare y x) (list y x)
      :else (list x y))) 
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

;;; simplify-sum-rec
(defn- simplify-sum-rec [op]
  (cond
   (and (= (count op) 2) (let [[x y] op] (not (or (= (kind x) :sumop) (= (kind y) :sumop))))) 
   (let [[x y] op]
     (cond
      (and (or (= (kind x) :number) (= (kind x) :fracop)) 
           (or (= (kind y) :number) (= (kind y) :fracop)))
      (let [p (simplify-rne (list '+ x y))]
        (if (= p 0) '() (list p)))
      (= x 0) (list y)
      (= y 0) (list x)
      (and (= (base x) (base y)) (= (exponent x) (exponent y))) (let [p (simplify-product (list 2 x))]
                                                                  (cond
                                                                   (= p 0) '()
                                                                   :else (list p)))
      ;; lex-less-than is the ordering constraint for the cannoican normal form
      (algebra-compare y x) (list y x)
      :else (list x y))) 
   ;; This is the else part and SPRDEC-2
   (and (= (count op) 2) (let [[x y] op] (or (= (kind x) :sumop) (= (kind y) :sumop))))
   (let [[x y] op]
     (cond
      (and (= (kind x) :sumop) (= (kind y) :sumop)) (merge-sums (rest x) (rest y))
      (= (kind x) :sumop) (merge-sums (rest x) (list y))
      (= (kind y) :sumop) (merge-sums (list x) (rest y))))
   (> (count op) 2) (let [w (simplify-sum-rec (rest op))] 
                      (cond
                       (= (kind (first op)) :sumop) (merge-sums (rest (first op)) w)
                       :else (merge-sums (list (first op)) w)))))

;;; simplify-product
(defn- simplify-product [u]
  (cond
   (some #(= 'UNDEFINED %) u) 'UNDEFINED
   (some #(= 0 %) u) 0
   (= (count u) 1) u
   :else (let [v (simplify-product-rec u)]
           (do 
             (cond
              (= (count v) 1) (first v)
              (= (count v) 0) 1
              :else (reduce #(list '* % %2) v))))))

;;; simplify-sum
(defn- simplify-sum [u]
  (cond
   (some #(= 'UNDEFINED %) u) 'UNDEFINED
   (= (count u) 1) u
   :else (let [v (simplify-sum-rec u)]
           (cond
            (= (count v) 1) (first v)
            (= (count v) 0) 0
            :else (reduce #(list '+ % %2) v)))))


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
                      [:sumop] (simplify-sum (rest v))
                      [:quotop] (simplify-quotient v)
                      [:diffop] (simplify-diff (rest v))
                      [:factop] (simplify-factorial v)
                      [_] (simplify-function v)))))

(defmacro simplify [& args]
  `(map simplify* '(~@args)))

;; (defn -main [& args]
;;   "I don't do a whole lot."
;;   (println args "Hello, World!"))


