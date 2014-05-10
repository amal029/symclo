(ns symclo.grobner
  (:gen-class))
(use '[clojure.core.match :only (match)])
(use 'clojure.math.numeric-tower)
(use 'clojure.tools.trace)
(require '[symclo.core :as simp])
(require '[symclo.expand :as expand])
(require '[symclo.rationalize :as natural])
(require '[symclo.util :as util])

(set! *assert* true)
(def ^:private third (comp first nnext))

(defn g-reduce
  "Reduction of u with symbols in L with respect to side relations F."
  [u F L] 
  (if (not= u 0) 
    (loop [R u
           r 0
           c (take (count F) (iterate identity 0))
           i 0
           m (util/lm R L)
           f (nth F i)
           Q (simp/simplify* (list '/ m (util/lm f L)))]
      (let [[R c i] (if (= (simp/kind (simp/simplify* (natural/denom Q))) :number)
                      [((comp simp/simplify* expand/expand* simp/simplify*) (list '- R (list '* Q f)))
                       (map-indexed #(if (= % i) (simp/simplify* (list '+ %2 Q)) %2) c)
                       0]
                      [R c (inc i)])
            [R r i] (if (= i (count F))
                      [(simp/simplify* (list '- R m)) (simp/simplify* (list '+ r m)) 0]
                      [R r i])]
        (if-not (= R 0) 
          (recur R r c i (util/lm R L) (nth F i) (simp/simplify* (list '/ (util/lm R L)  (util/lm (nth F i) L))))
          [(apply vector c) r]))) 
    [(apply vector (take (count F) (iterate identity 0))) 0]))

(defn s-poly 
  "Get the s-polynomial of u and v w.r.t ordered sequence L"
  [u v L]
  (let [d (util/mv-lcm (util/lm u L) (util/lm v L) L)
        f (simp/simplify* (expand/expand* (list '* u (first (util/mv-polynomial-division d (util/lm u L) L)))))
        s (simp/simplify* (expand/expand* (list '* v (first (util/mv-polynomial-division d (util/lm v L) L)))))]
    ;; FIXME: I need 2 simplifies sometimes, because the 0 identity in addition seems to not get deleted??
    (simp/simplify* (simp/simplify* (list '- f s)))))

(defn g-basis
  "Calculate the grobner basis given basis F and the ordered list of
   symbols L in F. Uses Bucherger's algorithm" 
  
  [F L]
  (if-not (empty? F) 
    (let [G F
          b (take (dec (count G)) (iterate identity G))
          c (map-indexed #(drop (inc %) %2) b)
          P (mapcat (fn [l m] (map #(vector % %2) (take (count m) (iterate identity l)) m)) (butlast G) c)]
      (loop [r (second (g-reduce (s-poly (first (first P)) (second (first P)) L) G L))
             P (rest P)
             G G]
        (let [[P G] (if-not (= r 0)
                      [(concat P (map #(vector % r) G))
                       (concat G [r])]
                      [P G])]
          (if-not (empty? P)
            (recur (second (g-reduce (s-poly (first (first P)) (second (first P)) L) G L)) 
                   (rest P)
                   G)
            G))))
    F))
