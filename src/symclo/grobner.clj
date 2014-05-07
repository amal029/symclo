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

(defn- g-reduce
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
      (let [[R c i] (if (= (simp/kind (natural/denom Q)) :number)
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

(defn- s-poly [u v L]
  (let [d (util/mv-lcm (util/lm u L) (util/lm v L))
        f (simp/simplify* (list '* u (list '/ d (util/lm u L))))
        s (simp/simplify* (list '* v (list '/ d (util/lm v L))))]
    (simp/simplify* (list '- f s))))

(defn g-basis
  "Calculate the grobner basis given basis F and the ordered list of
  symbols L in F" 
  
  [F L]
  (if-not (empty? F) 
    (let [G F
          b (take (dec (count G)) (iterate identity G))
          c (map-indexed #(drop (inc %) %2) b)
          P (mapcat (fn [l m] (map #(vector % %2) (take (count m) (iterate identity l)) m)) (butlast G) c)]
      (loop [s (s-poly (ffirst P) (ffirst P) L)
             P (rest P)
             r (second (g-reduce s G L))]
        (let [[P G] (if-not (= r 0)
                      [(concat P (map #(vector % r) G))
                       (concat G [r])]
                      [P G])]
          (if-not (empty? P)
            (recur (s-poly (ffirst P) (ffirst P) L) (rest P) (second (g-reduce (s-poly (ffirst P) (ffirst P) L) G L)))
            G))))
    F))
