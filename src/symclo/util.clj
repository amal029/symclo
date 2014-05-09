(ns symclo.util
  (:gen-class))
(use '[clojure.core.match :only (match)])
(use 'clojure.math.numeric-tower)
(use 'clojure.tools.trace)
(require '[symclo.trig :as trig])
(require '[symclo.core :as simp])
(require '[symclo.expand :as expand])

(set! *assert* true)
(declare G)
(def ^:private third (comp first nnext))

(declare mv-gcd)

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

(defn substitute 
  "Substitue expression x with y in expression f."
  
  [f x y]
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

(defn free-of 
  "Expression u is free-of expression x"
  
  [u x]
  (not (some (partial = x) (complete-sub-expression u))))

(defn- monomial-gpe*
  [v u] 
  (cond
   (contains? v u) true
   (= (simp/kind u) :powop)
   (and (contains? v (second u)) (integer? (third u)) (> (third u) 0))
   (= (simp/kind u) :prodop)
   (reduce #(and % %2) (map (partial monomial-gpe* v) (simp/get-prod-operands (rest u)))) 
   :else (every? (partial free-of u) v)))

(defn monomial-gpe 
  "Function that recognizes if u is a generalized monomial expression in
  variable(s) v. v can be a set" 
  [u v]
  (monomial-gpe* (if-not (set? v) #{v} v) (simp/simplify* u)))

(defn- polynomial-gpe* [v u]
  (cond
   (not= (simp/kind u) :sumop)
   (monomial-gpe* v u)
   (contains? v u) true
   :else 
   (reduce #(and % %2) (map (partial monomial-gpe* v) (simp/get-sum-operands (rest u))))))

(defn polynomial-gpe
  "Function that recognizes if u is a generalized polynomial expression
  in variable(s) v. v can be a set" 
  [u v] 
  (polynomial-gpe* (if-not (set? v) #{v} v) (simp/simplify* u)))


(defn- degree-monomial* [v u]
  (cond
   (contains? v u) 1
   (= (simp/kind u) :powop)
   (if (and (contains? v (second u)) (integer? (third u)) (> (third u) 0))
     (third u) 0)
   (= (simp/kind u) :prodop)
   (let [vs (map (partial degree-monomial* v) (simp/get-prod-operands (rest u)))]
     (if-not (some (partial = 'UNDEFINED) vs) (reduce + vs) 'UNDEFINED)) 
   :else (if (reduce #(and % %2) (map (partial free-of u) v)) 0 'UNDEFINED)))

(defn degree-monomial 
  "Function that gives the degree of the generalized monomial expression
  in variable(s) v. v can be a set" 
  [u v]
  (degree-monomial* (if-not (set? v) #{v} v) (simp/simplify* u)))

(defn- degree-polynomial* [v u]
  (cond
   (not= (simp/kind u) :sumop)
   (degree-monomial* v u)
   (contains? v u) 0
   :else 
   (apply max (map (partial degree-monomial* v) (simp/get-sum-operands (rest u))))))

(defn degree-polynomial
  "Function that gives the degree of the generalized polynomial expression
  in variable(s) v. v can be a set"
  [u v] 
  (degree-polynomial* (if-not (set? v) #{v} v) (simp/simplify* u)))

(defn coefficient-monomial-gpe 
  "Function that gives the list of coefficients of a generalized
   monomial expression (GME) x: list, u: GME" 
  [u x]
  (cond
   (= u x) [1 1]
   (= (simp/kind u) :powop)
   (if (and (= (second u) x) (= (simp/kind (third u)) :number) (> (third u) 0)) [1 (third u)] 
       (if (free-of u x) [u 0] 'UNDEFINED))
   (= (simp/kind u) :prodop)
   (let [va (map #(coefficient-monomial-gpe % x) (simp/get-prod-operands (rest u)))]
     (if (some (partial = 'UNDEFINED) va)
       'UNDEFINED
       (let [va (second (last (filter #(not= (second %) 0) va)))]
         (if-not (nil? va)
           [(simp/simplify* (list '/ u (list '** x va))) va]
           [u 0]))))
   (free-of u x) [u 0]
   :else 'UNDEFINED))

(defn coefficient-polynomial-gpe 
  "Function that gives the coefficient of a generalized polynomial
   expression (GPE) x: list, u: GPE, j: non negative degree" 
  
  [u x j]
  (cond
   (not= (simp/kind u) :sumop)
   (let [v (coefficient-monomial-gpe u x)]
     (if (= v 'UNDEFINED) 
       'UNDEFINED 
       (if (= (second v) j) (first v) 0)))
   (= u x)
   (if (= j 1) 1 0)
   :else
   (let [v (map #(coefficient-monomial-gpe % x) (simp/get-sum-operands (rest u)))]
     (cond 
      (some (partial = 'UNDEFINED) v) 
      'UNDEFINED
      :else 
      (let [c (map #(first %) (filter #(= (second %) j) v))]
        (if (empty? c) 0 (simp/simplify* (reduce #(list '+ % %2) c))))))))

(defn polynomial-lce 
  "The leading coefficient of polynomial u w.r.t symbol x"
  [u x]
  (coefficient-polynomial-gpe u x (degree-polynomial u x)))

(defn polynomial-division 
  "Divides univariate polynomials u by v, w.r.t. x a symbol" 
  [u v x]
  (loop [q 0
         r u
         m (degree-polynomial r x)
         n (degree-polynomial v x)
         lcv (polynomial-lce v x)]
    (if (and (not= r 0) (>= m n))
      ;; then
      (let [lcr (polynomial-lce r x)
            s (simp/simplify* (list '/ lcr lcv))
            q (simp/simplify* (list '+ q (list '* s (list '** x (list '- m n)))))
            r (simp/simplify* (expand/expand* (simp/simplify* (list '- (list '- r (list '* (list '** x m) lcr))
                                                                    (list '* (list '* s (list '** x (list '- m n)))
                                                                          (list '- v (list '* (list '** x n) lcv)))))))
            m (degree-polynomial r x)]
        (recur q r m n lcv))
      ;; else
      [q r])))

(defn polynomial-quotient 
  "Performs univariate polynomial division and gives the quotient back."
  [u v x]
  (first (polynomial-division u v x)))

(defn polynomial-remainder 
  "Performs univariate polynomial division and gives the remainder
  back."  
  [u v x]
  (second (polynomial-division u v x)))

(defn polynomial-expansion* 
  "The polynomial expansion of u in terms of v involves the
   representation of u as a sum whose terms contain non-negative integer
   powers of v. Expansion happens w.r.t symbols x and t. t can be any
   symbol not in u or v" 
  [u v x t]
  (if (= u 0) 
    0
    (let [d (polynomial-division u v x)]
      (simp/simplify* (expand/expand* (simp/simplify* (list '+ (second d) (list '* t (polynomial-expansion* (first d) v x t)))))))))


(defn polynomial-expansion
  "The polynomial expansion of u in terms of v involves the
   representation of u as a sum whose terms contain non-negative integer
   powers of v. Expansion happens w.r.t symbols x. Do not use the
   literal symbol t in v or u" 
  [u v x]
  (substitute (polynomial-expansion* u v x 't) 't v))

(defn lm 
  "Get the leading monomial coefficient from a mutilvariate polynomial
  expression u given the list of symbols l. Where (first l) is the
  leading symbol" 
  
  [u l]
  (cond
   (empty? l) u
   :else
   (let [m (degree-polynomial u (first l))
         c (coefficient-polynomial-gpe u (first l) m)]
     (simp/simplify* (list '* (list '** (first l) m) (lm c (rest l)))))))

(defn mv-rec-polynomial-div
  "Multivariate recursive structure based polynomial division. Monomial
  based division via mv-polynomial-division should be prefered."  
  
  [u v l]
  (cond
   (empty? l) [(simp/simplify* (list '* u (list '** v -1))) 0]
   :else
   (loop [x (first l)
          r u
          m (degree-polynomial r x)
          n (degree-polynomial v x)
          q 0
          lcv (polynomial-lce v x)
          d [0 0]]
     (if (and (not= r 0) (>= m n))
       (let [lcr (simp/simplify* (polynomial-lce r x))
             d (mv-rec-polynomial-div lcr lcv (rest l))
             [q r m] (if (not= (second d) 0)
                       [q r m]
                       [(simp/simplify* (list '+ (list '* (first d) (list '** x (- m n))) q)) ;q
                        (simp/simplify* (expand/expand* (simp/simplify* (list '- r (list '* v (first d) (list '** x (- m n))))))) ;r
                        ;; this is a bit sucky, FIXME!
                        (degree-polynomial (simp/simplify* (expand/expand* (simp/simplify* (list '- r (list '* v (first d) (list '** x (- m n))))))) x) ;m
                        ])]
         (if (and (not= r 0) (= (second d) 0)) 
           (recur x r m n q lcv d)
           [(simp/simplify* (expand/expand* (simp/simplify* q))) r]))
       [(simp/simplify* (expand/expand* (simp/simplify* q))) r]))))

(defn- G 
  "u is a multivariate GPE, i.e., a sum of multivariate GME(s) and v is
  a multivariate GME" 
  [u v l] 
  {:pre [(polynomial-gpe u (set l))
         (monomial-gpe v (set l))]}
  (cond
   (= (simp/kind u) :sumop)
   (let [ops (simp/get-sum-operands (rest u))
         res (filter #(= 0 (second %)) (map #(mv-rec-polynomial-div % v l) ops))]
     (if (empty? res)
       0
       (simp/simplify* (reduce #(list '+ % (first %2)) 0 res))))
   :else (first (mv-rec-polynomial-div u v l))))

(defn mv-polynomial-division
  "Multivariate polynomial division. u gets divided by v using monomial
  division. l is the list of symbols, (first l) is the main
  symbol. Returns a 2 vector, first is the quotient and second is the
  remainder."  
  [u v l]
  (loop [q 0
         r u
         vl (lm v l)
         f (G r vl l)]
    (let [q (simp/simplify* (list '+ q f))
          r (simp/simplify* (expand/expand* (simp/simplify* (list '- r (list '* f v)))))
          f (G r vl l)]
      (if-not (= 0 f) (recur q r vl f) [q r]))))

(defn- normalize* [u L K]
  (cond
   (and (= K 'Z) (= (simp/kind u) :number))
   (cond 
    (> u 0) 1
    (= u 0) 0
    :else -1)
   (and (= K 'Z) (= (simp/kind u) :fracop))
   (throw (Throwable. (str "Fraction in a integer domain: " u)))
   (and (= K 'Q) (or (= (simp/kind u) :number) (= (simp/kind u) :fracop)))
   (simp/simplify* (list '** u -1))
   :else
   (recur (polynomial-lce u (first L)) (rest L) K)))

(defn normalize
  "Normalize multivariate polynomial u in Q domain to a monic polynomial
   or 0 and when in Z domain, into the unit normal form."
  
  [u L K]
  (let [v (normalize* u L K)]
    (simp/simplify* (list '* v u))))

(defn- polynomial-content [u x R K]
  (let [alldegrees (concat (range (degree-polynomial u x) 0 -1) [0])
        coefs (filter (partial not= 0) (map (partial coefficient-polynomial-gpe u x) alldegrees))]
    (cond
     (>= (count coefs) 2) 
     (reduce #(mv-gcd % %2 R K) 0 coefs)
     (= (count coefs) 1)
     (normalize (first coefs) R K)
     :else 0)))

(defn- pseudo-remainder [u v x]
  (loop [p 0
         s u
         m (degree-polynomial s x)
         n (degree-polynomial v x)
         delta (max (inc (- m n)) 0)
         lcv (coefficient-polynomial-gpe v x n)
         sigma 0]
    (if (and (not= s 0) (>= m n))
      (let [lcs (coefficient-polynomial-gpe s x m)
            p (simp/simplify* (list '+ (list '* lcv p) (list '* lcs (list '** x (list '- m n)))))
            s ((comp simp/simplify* expand/expand* simp/simplify*)
               (list '- (list '* lcv s) (list '* lcs v (list '** x (list '- m n)))))
            sigma (inc sigma)
            m (degree-polynomial s x)]
        (recur p s m n delta lcv sigma))
      ((comp simp/simplify* expand/expand* simp/simplify*) (list '* s (list '** lcv (list '- delta sigma)))))))

(defn- mv-poly-gcd-rec [u v L K]
  (cond
   (empty? L) (if (= K 'Z) (simp/integer_gcd u v) 1)
   :else
   (let [x (first L)
         R (rest L)
         cont_u (polynomial-content u x R K)
         cont_v (polynomial-content v x R K)
         d (mv-poly-gcd-rec cont_u cont_v R K)]
     (loop [pp_u (first (mv-rec-polynomial-div u cont_u L))
            pp_v (first (mv-rec-polynomial-div v cont_v L))]
       (if-not (= 0 pp_v)
         (let [r (pseudo-remainder pp_u pp_v x)
               pp_r (if (= r 0) 0
                        (first (mv-rec-polynomial-div r (polynomial-content r x R K) L)))]
           ;; FIXME: this can be made better
           (if-not (= 0 pp_v) (recur pp_v pp_r)
                   ((comp simp/simplify* expand/expand* simp/simplify*) (list '* d pp_u))))
         ((comp simp/simplify* expand/expand* simp/simplify*) (list '* d pp_u)))))))

(defn mv-gcd 
  "Multivariate polynomial gcd of u and v, given the order of symbols in
   list L and coefficient field K is Z or Q"
  
  [u v L K]
  (cond
   (= u 0) (simp/simplify* (normalize v L K))
   (= v 0) (simp/simplify* (normalize u L K))
   :else (simp/simplify* (normalize (mv-poly-gcd-rec u v L K) L K))))

(defn mv-lcm 
  "Multivariate polynomial lcm of u and v, given the order of symbols in
   list L and coefficient field K is Z or Q"
  ([u v L K] (simp/simplify* (list '/ (list '* u v) (mv-gcd u v L K))))
  ([u v L] (simp/simplify* (list '/ (list '* u v) (mv-gcd u v L 'Q)))))

(defn back-substitute 
  "Given an upper triangular matrix (alist = a list of lists) performs
   back substitution. Used for solving simultaneous equations."
  
  [alist symbols]
  (let [solns [(simp/simplify* (list '* -1 (last (last alist))))]]
    (loop [solns solns
           alist (drop-last alist)]
      (if-not (empty? alist)
        (let [row (drop (- (count symbols) (count solns)) (last alist))
              ll (last row)
              row (map #(simp/simplify* (list '* % %2)) (drop-last row) (reverse solns))
              solns (conj solns (simp/simplify* (list '* -1 (reduce #(simp/simplify* (list '+ % %2)) ll row))))]
          (recur solns (drop-last alist)))
        (interleave (reverse symbols) solns)))))

(defn- swap [alist i toswaprow]
  (map-indexed #(if (= % i) toswaprow %2)))

(defn solve-linear-eqs
  "Solve linear equations using Gauss-Jordan elimination. eqs and
  symbols are sets of linear equations with their symbols,
  respectively. Example: 
  (def e (list '= (simplify* '(- x y)) '(+ 3 z))) 
  (def q (list '= (simplify* '(+ x y)) '(+ 2 z)))
  (util/solve-linear-eqs #{e q} #{'x 'y})"
  
  [eqs symbols]
  
  {:pre [(set? eqs) (set? symbols) (every? #(= (first %) '=) eqs)
         (every? #(= (degree-polynomial % symbols) 1) (map second eqs))]}
  
  ;; First make a 2-d Augmented array
  (let
      [leqs (map #(simp/simplify* (list '* -1 %)) (map third eqs))
       eqs (map second eqs)
       alist (map (fn [eq] (map #(coefficient-polynomial-gpe eq % 1) symbols)) eqs)
       alist (map-indexed #(reverse (cons (nth leqs %) (reverse %2))) alist)]
    (loop [i 0 j 0
           alist alist]
      (cond
       (not= (nth (nth alist i) j) 0)
       ;; step-2a
       (let [element (nth (nth alist i) j)
             alist-current-row (map #(simp/simplify* (list '/ % element)) (nth alist i))
             alist (map-indexed (fn [idx row] (if (> idx i)
                                                (let [X (nth row j)
                                                      acr (map #(simp/simplify* (list '* -1 X %)) alist-current-row)
                                                      pa-to-add (map #(list % %2) acr row)]
                                                  (map-indexed #(if (>= % j)
                                                                  (simp/simplify* (list '+ (first %2) (second %2)))
                                                                  (second %2)) pa-to-add))
                                                row)) alist) 
             alist (map-indexed #(if (= % i) alist-current-row %2) alist)
             i (inc i) j (inc j)]
         (if (and (< j (count (first alist))) (< i (count alist)))
           (recur i j alist)
           (back-substitute alist symbols)))
       :else 
       ;; step-2b
       (let [toswaprow (map-indexed (fn [idx row] (if (and (> idx i) (not= (nth row j) 0)) row nil)) alist)
             toswaprow (first (filter (partial not= nil) toswaprow))]
         (if (nil? toswaprow)
           ;; step-2bi
           (let [j (inc j)]
             (if (and (< j (count (first alist))) (< i (count alist)))
               (recur i j alist)
               (back-substitute alist symbols)))
           ;; step-2bii
           (recur i j (swap alist i toswaprow))))))))
