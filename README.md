# symclo

A Clojure library for computer algebra systems a la, MAPEL, MAXIMA, MATHEMATICA,...

## API
http://amal029.github.io/symclo/index.html

## Current status

Simplification of rational polynomials

Simplification of trignometric expressions

Expansion of polynomials with rational coefficients

Rationalization of polynomials with rational coefficients

Simple derivates for polynomials

Simple integration of polynomials

Substitution and expression solving

Coefficient and degree detection

Univariate Polynomial division

Univariate Polynomial expansion

Simultaneous linear equation solver

Multivariate Polynomial division

Multivariate Polynomial gcd

Multivariate Polynomial lcm

Groebner basis calculation in Q and Z domains

Multivariate Polynomial expansion (untested)

# TODO

~~Automatic trigonometric simplification algorithm~~

Polynomial factorization

Simplification of polynomials with imaginary coefficients.

~~Elementary Polynomial integration~~
~~Integration of polynomials in rational form~~

Integration of polynomials in radical form

Integration of polynomials with complex trignometric expressions

~~Polynomial differentiation (currently differentiation is available in Incanter)~~

## Usage

* (simplify (expr)*): simplify expression
* (expand (expr)*): expand expression
* (trig-simplify (expr)*): trignometric simplification
* (natural (expr)*): rationalization
* (deriv [expr symbol]): derivate with respect to symbol
* (integrate [expr symbol]): integrate with respect to symbol


# Operators:
* '* multiply
* '** exponentiation
* '+ addition
* '- subtraction
* '/ quotient or fraction
* '! factorial
* 'sin sine
* 'cos cosine
* 'tan tangent
* 'cot cotangent
* 'sec secant
* 'csc cosecant
* '%pi PI
* '%ln natural logarithm
* '%e the "e" operator

# Example REPL session
``` clojure
symclo.core> (simplify (* a b (** a (/ 1 2))))

((* (** a (/ 3 2)) b))

symclo.core> (simplify (+ a a b))

((+ (* 2 a) b))

symclo.core> (simplify (+ (** (/ 1 2) -2) a b))

((+ (+ 4 a) b))

symclo.core> (simplify (/ (/ a b) a))

((** b -1))

symclo.core> (simplify (** a (* -1 (! 5))))

((** a -120))

symclo.core> (simplify (/ a (! 5)))

((* (** 120 -1) a))
```

### Do multiple expressions together
``` clojure
symclo.core> (simplify (/ (/ a b) a) (** (/ 1 x) -2))

((** b -1) (** x 2))

symclo.core> (simplify (* (+ a b) (+ a b)) (* (- a b) (- a b)))

((** (+ a b) 2) (** (+ a (* -1 b)) 2))

symclo.core> (simplify (+ (+ a b) (+ a b)) (+ (- a b) (- a b)))

((+ (* 2 a) (* 2 b)) (+ (* 2 a) (* -2 b)))

symclo.core> (simplify (+ (+ a b) (+ a b)) (+ a b a b))

((+ (* 2 a) (* 2 b)) (+ (* 2 a) (* 2 b)))
```

### Check equality of two algebraic expressions
``` clojure
symclo.core> (= (simplify (+ a b a b)) (simplify (+ (+ a b) (+ a b))))

true
```

### Expand expressions

``` clojure
symclo.core> (expand/expand (* (- x y) (+ x y)))

((+ (+ (+ (** x 2) (* (* -1 x) y)) (* x y)) (* -1 (** y 2))))


symclo.core> (expand/expand (** (- x y) 2))

((+ (+ (** x 2) (* (* -2 x) y)) (** y 2)))


symclo.core> (expand/expand (** (+ x y) 3))

((+ (+ (+ (* (* 3 x) (** y 2)) (** x 3)) (* (* 3 (** x 2)) y)) (** y 3)))

```

### Check equality of two expanded algebraic expressions
``` clojure
symclo.core> (= (expand/expand (** (+ x y) 2)) (expand/expand (* (+ x y) (+ x y))))
true
```

#### Automatic simplification of trignometric expressions
``` clojure
symclo.core> (trig/trig-simplify (+ (** (sin x) 2) (** (cos x) 2)))
(1)

symclo.core> (trig/trig-simplify (** (tan (/ %pi 3)) 4))

(9)


symclo.core>  (trig/trig-simplify (+ (** (tan (/ %pi 3)) 3) (** (sin (/ %pi 3)) 3)))

((* (/ 9 8) (** 3 (/ 3 2))))


symclo.core> (trig/trig-simplify (+ (cos (* 2 a)) (** (sin a) 2)))

((+ (/ 1 2) (* (/ 1 2) (cos (* 2 a)))))

symclo.core>  (trig/trig-simplify (+ (** (tan x) 3) (** (cot x) 3)))

((* (+ (+ (+ (* (* (* (** (sin x) 2) (sin (* 3 x))) (/ -1 4)) (sin x)) (* (* (* (** (cos x) 2) (cos (* 3 x))) (/ 1 4)) (cos x))) (* (/ 3 4) (** (cos x) 4))) (* (/ 3 4) (** (sin x) 4))) (** (+ (+ (+ (+ (+ (+ (+ (+ (* (* (/ 1 4) (cos x)) (sin x)) (* (* (/ -1 16) (cos x)) (sin (* 3 x)))) (* (* (/ 1 8) (cos x)) (sin x))) (* (* (/ 1 8) (cos (* 3 x))) (sin x))) (* (* (/ 1 8) (cos x)) (sin x))) (* (* (/ 1 16) (cos (* 3 x))) (sin x))) (* (* (/ -1 8) (cos x)) (sin (* 3 x)))) (* (* (/ 1 16) (cos x)) (sin x))) (* (* (/ -1 16) (cos (* 3 x))) (sin (* 3 x)))) -1)))

```


### Trignometric simplification 
#### (Manual -- use it in style of theorem proving)
``` clojure
symclo.core> (simplify* `(~'+ ~(trig/tr5 '(** (sin x) 2)) (~'** (~'cos ~'x) 2)))

1

symclo.core> (simplify* `(~'+ ~(trig/tr11 '(cos (* 2 a))) ~(trig/tr5 '(** (sin a) 2))))

(** (cos a) 2)
```

### Check equality of two trignometric expressions (manually or automatically)
``` clojure
symclo.core> (= 
	     (simplify* (trig/tr4 '(tan (/ %pi 3)))) 
	     (simplify* (list '/ (trig/tr4 '(sin (/ %pi 3))) (trig/tr4 '(cos (/ %pi 3))))))

true


symclo.core> (= (trig/trig-simplify (/ (sin (* 4 a)) (cos (* 4 a)))) (trig/trig-simplify (tan (* 4 a))))

true

```
#### Rationalization of polynomials

``` clojure

symclo.core>  (natural/natural (+ (/ 1 x) (/ 1 y)))

((* (* (** x -1) (** y -1)) (+ x y)))


symclo.core> (def a (natural/natural* '(+ (/ 1 x) (/ 1 y))))

#'symclo.core/a

symclo.core> (simplify* (natural/denom a))

(* x y)

symclo.core> (simplify* (natural/numer a))

(+ x y)

```
#### Derivatives

``` clojure 
symclo.core> (deriv/deriv x x)

1

symclo.core> (deriv/deriv (+ x 2) x)

1

symclo.core> (deriv/deriv* '(* x 2) 'x)

2

symclo.core> (deriv/deriv (** x 2) x)

(* 2 x)

symclo.core> (deriv/deriv (** x (** x 2)) x)

(+ (** x (+ 1 (** x 2))) (* (* 2 (%ln x)) (** x (+ 1 (** x 2)))))

symclo.core> (deriv/deriv (** (+ x y) 4) x)

(* (* 4 (** (+ x y) 3)) (+ 4 (* 4 (%deriv y x))))

symclo.core> (simplify* (deriv/deriv* (expand/expand* '(** (+ x y) 4)) 'x))

(+ (+ (+ (+ (+ (+ (+ (* 4 (** y 3)) (* (* 12 x) (** y 2))) (* (* 4 (%deriv y x)) (** x 3))) (* 4 (** x 3))) (* (* (* (** x 2) 12) (%deriv y x)) y)) (* (* 12 (** x 2)) y)) (* (* (* x 12) (%deriv y x)) (** y 2))) (* (* 4 (%deriv y x)) (** y 3)))

symclo.core> (def a (trig/trig-simplify* '(+ (cos (* 2 a)) (** (sin a) 2))))

#'symclo.core/a

symclo.core> a

(+ (/ 1 2) (* (/ 1 2) (cos (* 2 a))))

symclo.core> (deriv/deriv* a 'a)
(* -1 (sin (* 2 a)))

;;; Derivative of complex trignometric functions needs pre-processing step

;;; tan(x)' cannot be solved by just using the derivative
symclo.core> (deriv/deriv* '(tan x) 'x)

(%deriv (tan x) x)

;;; tan(x)' can be solved using trig-simplification and then deriving that 
symclo.core> (trig/trig-simplify* (deriv/deriv* (trig/trig-simplify* '(tan x)) 'x))

(** (+ (/ 1 2) (* (/ 1 2) (cos (* 2 x)))) -1)

```

#### Integration of elementary polynomials

``` clojure
symclo.core> (integrate/integrate 1 x)

x

symclo.core> (integrate/integrate x x)

(* (/ 1 2) (** x 2))

symclo.core> (integrate/integrate (** x 3) x)

(* (/ 1 4) (** x 4))

symclo.core> (integrate/integrate (** %e x) x)

(** %e x)

symclo.core> (integrate/integrate (* 2 x (cos (** x 2))) x)

(sin (** x 2))

symclo.core> (integrate/integrate (* (+ (* 2 x) 1) (cos (+ (** x 2) x))) x)

(sin (+ x (** x 2)))

symclo.core> (integrate/integrate (* (+ x 1) (+ x 2)) x)

(+ (+ (* 2 x) (* (/ 3 2) (** x 2))) (* (/ 1 3) (** x 3)))

symclo.core> (integrate/integrate (** (+ x 1) 2) x)

(* (/ 1 3) (** (+ 1 x) 3))


symclo.core> (integrate/integrate (** x -1) x)

(%ln x)

;;; Integrating polynomials in rational form

symclo.core>  (integrate/integrate* (simplify* '(/ (cos x) (+ (** (sin x) 2) (* 3 (sin x)) 4))) 'x)

(* (* 2 (** 7 (/ -1 2))) (arctan (* (** 7 (/ -1 2)) (+ 3 (* 2 (sin x))))))

```

#### Checking if the integrals are actually correct

``` clojure

symclo.core> (def a (simplify* '(* (+ (* 2 x) 1) (cos (+ (** x 2) x)))))

#'symclo.core/a
symclo.core> a

(* (cos (+ x (** x 2))) (+ 1 (* 2 x)))

symclo.core> (def b  (simplify* (integrate/integrate* a 'x)))

#'symclo.core/b
symclo.core> b

(sin (+ x (** x 2)))

;;; checking with db/dx
symclo.core> (= a (simplify* (deriv/deriv* b 'x)))

true

```

#### Substitute and solve

``` clojure

symclo.core> (simplify* (util/substitute '(+ x 1) 'x 1))

2

symclo.core> (simplify* (util/substitute (simplify* '(* x (+ x y))) 'x 2))

(+ 4 (* 2 y))

;;; composition of substitute and solve -- note that composition of substitution is not commutative
symclo.core> (simplify* (util/substitute (util/substitute (simplify* '(* x (+ x y))) 'x 2) '(+ x y) 3))

(+ 4 (* 2 y))

symclo.core> (simplify* (util/substitute (util/substitute (simplify* '(* x (+ x y))) '(+ x y) 3) 'x 2))

6
```

#### Utility functions for polynomials

``` clojure
;;; Degree of polynomials -- argument 1 is the polynomial and argument 2 is the set (or a single symbol)
;;; with respect to which the degree needs to be found out.
symclo.core> (util/degree-polynomial (simplify* '(* 3 w (** x 2) (** y 3) (** z 4))) #{'x 'z})

6

symclo.core> (util/degree-polynomial (simplify* '(+ (* 2 (** x 2) y (** z 3)) (* w x (** z 6)))) #{'x 'z})

7

;;; Coefficients of polynomials arg-1 is the polynomial, arg-2 is the
;;; symbol and arg-3 is the degree whose coefficient we want.

symclo.core> (util/coefficient-polynomial-gpe (simplify* '(+ (* 3 x (** y 2)) (* 5 (** x 2) y) (* 7 x) 9)) 'x 1)

(+ 7 (* (* (* x (** y 2)) 3) (** x -1)))

;;; Nesting of the coefficient operator to find the coefficient of '(* x (** y 2))

symclo.core> (util/coefficient-polynomial-gpe (simplify* (util/coefficient-polynomial-gpe (simplify* '(+ (* 3 x (** y 2)) (* 5 (** x 2) y) (* 7 x) 9)) 'x 1)) 'y 2)

3

;;; Nesting to find the leading coefficient

symclo.core> (def u (simplify* '(+ (* 3 x (** y 2)) (* 5 (** x 2) y) (* 7 x) 9))) 

#'symclo.core/u
symclo.core> u
(+ (+ (+ 9 (* 7 x)) (* (* 5 (** x 2)) y)) (* (* 3 x) (** y 2)))
symclo.core> (util/coefficient-polynomial-gpe u 'x (util/degree-polynomial u 'x))
(* (* (* (** x 2) y) 5) (** x -2))

```

#### Polynomial division and expansion

``` clojure 

;;; Note this is univariate polynomial division
symclo.core> (util/polynomial-division (simplify* '(- (** x 2) 1)) (simplify* '(- x 1)) 'x)

[(+ 1 x) 0]

symclo.core> (util/polynomial-division (simplify* '(+ (* 5 (** x 2)) (* 4 x) 1)) (simplify* '(+ (* 2 x) 3)) 'x)

[(+ (/ -7 4) (* (/ 5 2) x)) (/ 25 4)]

;;; Polynomial expansion is a generalization of substitution

symclo.core> u                                                                                                                                                              

(+ (+ 7 (* 5 (** x 2))) (* 3 (** x 4)))

;;; Incorrect degree! (should be 2, see equation u above)
symclo.core> (util/degree-polynomial u '(** x 2))

1

;;; Correct degree!!
symclo.core> (util/degree-polynomial (util/polynomial-expansion* u '(** x 2) 'x 't) 't)                                                                  

2

;;; Incorrect coefficent!! (should be 3, see equation u above)
symclo.core> (util/coefficient-polynomial-gpe u '(** x 2) 2)                                                                                             

0

;;; Correct coefficient!
symclo.core> (util/coefficient-polynomial-gpe (util/polynomial-expansion* u '(** x 2) 'x 't) 't 2) 

3

```

#### Solve simultaneous linear equations
``` clojure
symclo.core> eq1

(+ x (* 2 y))

symclo.core> eq2

(+ x (* -1 y))

;;; result
symclo.core> (util/solve-linear-eqs #{`(~'= ~eq1 1) `(~'= ~eq2 0)} #{'x 'y})

(x (/ 1 3) y (/ 1 3))

symclo.core> r1 

(+ (* 2 x) y)

symclo.core> r2

(+ x (* -1 y))

symclo.core> (util/solve-linear-eqs #{`(~'= ~r1 4) `(~'= ~r2 0)} #{'x 'y}) 

(x (/ 4 3) y (/ 4 3))



;;; Building the list equation from expression
symclo.core> (def q (list '= (simplify* '(+ x y)) '(+ 2 z)))

#'symclo.core/q

symclo.core> q

(= (+ x y) (+ 2 z))

symclo.core> (def e (list '= (simplify* '(- x y)) '(+ 3 z)))

#'symclo.core/e

symclo.core> e

(= (+ x (* -1 y)) (+ 3 z))

symclo.core> (util/solve-linear-eqs #{e q} #{'x 'y})                                                                                                                        
(x (+ (/ 5 2) z) y (/ -1 2))

```
#### Multivariate polynomial division and expansion

``` clojure

symclo.core> (def x (simplify* '(+ (* 2 (** x 2) y) (* 3 (** x 2)) (* 4 x y) (* 5 x) (* 6 y) 7)))

#'symclo.core/x

symclo.core> x

(+ (+ (+ (+ (+ 7 (* 5 x)) (* 3 (** x 2))) (* 6 y)) (* (* 4 x) y)) (* (* 2 (** x 2)) y))

symclo.core> (def v (simplify* '(* x y)))

#'symclo.core/v

symclo.core> v

(* x y)

;;; The first value is the quotient and second is the remainder
;;; multivariante polynomial division uses monomial based division 
;;; internally
symclo.core>  (util/mv-polynomial-division x v ['x 'y])

[(+ 4 (* 2 x)) (+ (+ (+ 7 (* 5 x)) (* 3 (** x 2))) (* 6 y))]

```

#### Multivariate polynomial gcd

``` clojure


symclo.core> (def v (simplify* '(+ (* y (** x 2)) (* 2 (** y 2) x) (** y 3))))
#'symclo.core/v
symclo.core> v
(+ (+ (* (** x 2) y) (* (* 2 x) (** y 2))) (** y 3))

symclo.core> (def u (simplify* '(+ (** y 3) (* -1 y (** x 2)))))
#'symclo.core/u
symclo.core> u
(+ (* (* -1 (** x 2)) y) (** y 3))

symclo.core> (util/mv-gcd u v ['x 'y] 'Z)
(+ (* x y) (** y 2))

symclo.core> (util/mv-lcm u v ['x 'y] 'Z)

(* (* (** (+ (* x y) (** y 2)) -1) (+ (* (* -1 (** x 2)) y) (** y 3))) (+ (+ (* (** x 2) y) (* (* 2 x) (** y 2))) (** y 3)))

```

#### Grobner basis

``` clojure

symclo.core=> (def f1 (simplify* '(+ (* x y) x 1)))
#'symclo.core/f1

symclo.core=> (def f2 (simplify* '(+ 1 y (** x 2) (* x (** y 2)))))
#'symclo.core/f2

;;; The grobner basis calculation
symclo.core=> (time (grobner/g-basis [f1 f2] ['x 'y]))
"Elapsed time: 424.598812 msecs"
((+ (+ 1 x) (* x y)) (+ (+ (+ 1 (** x 2)) y) (* x (** y 2))) (+ (+ -1 x) (* -2 y)) (+ (+ 2 (* 3 y)) (* 2 (** y 2))))

;;; Grobner basis calculation is not always the fastest!
symclo.core=> (def f1 (simplify* '(+ 6 (* 3 (** x 2)) (* 2 y (** x 3)))))
#'symclo.core/f1

symclo.core=> (def f2 (simplify* '(+ 1 y x (* 5 (** x 2)) (* 3 (** y 2)))))
#'symclo.core/f2

;;; The grobner basis calculation
symclo.core=> (time (grobner/g-basis [f1 f2] ['x 'y]))
"Elapsed time: 314281.473265 msecs"
((+ (+ 6 (* 3 (** x 2))) (* (* 2 (** x 3)) y)) (+ (+ (+ (+ 1 x) (* 5 (** x 2))) y) (* 3 (** y 2))) (+ (+ (+ (+ (+ (+ (+ 27 (* -3 x)) (* (/ -13 5) y)) (* (* (/ -8 5) x) y)) (* (/ -43 5) (** y 2))) (* (* -2 x) (** y 2))) (* (/ 6 5) (** y 3))) (* (* -6 x) (** y 3))) (+ (+ (+ (+ (+ (+ (+ (+ (/ -114 -25) (* (/ -264 -25) x)) (* (/ 6 -125) y)) (* (* (/ 146 -125) x) y)) (* (/ 46 -125) (** y 2))) (* (* (/ 18 -5) x) (** y 2))) (* (/ -152 -125) (** y 3))) (* (/ 24 25) (** y 4))) (* (/ 36 25) (** y 5))) (+ (+ (+ (+ (+ (+ (+ (+ (+ (/ 7002 625) (* (/ 25782 625) x)) (* (/ -53728 3125) y)) (* (* (/ -7102 -3125) x) y)) (* (/ -24752 -3125) (** y 2))) (* (/ -32426 -3125) (** y 3))) (* (/ -1792 -625) (** y 4))) (* (/ 4608 625) (** y 5))) (* (/ 216 125) (** y 6))) (* (/ 216 125) (** y 7))) (+ (+ (+ (+ (+ (+ (+ (+ (/ -194143594 443875) (* (/ -110269094 88775) x)) (* (/ -744581198 -1331625) y)) (* (/ -3729058 17755) (** y 2))) (* (/ -142822904 443875) (** y 3))) (* (/ 106233232 -1331625) (** y 4))) (* (/ 96852288 -443875) (** y 5))) (* (/ -19471896 443875) (** y 6))) (* (/ -23306976 443875) (** y 7))) (+ (+ (+ (+ (+ (+ (+ (+ (+ (/ 2988 125) (* (/ -42228 -625) y)) (* (/ -16628 625) (** y 2))) (* (/ -23436 625) (** y 3))) (* (/ -11028 -625) (** y 4))) (* (/ 5092 625) (** y 5))) (* (/ -168 -125) (** y 6))) (* (/ 432 125) (** y 7))) (* (/ 1728 625) (** y 8))) (* (/ 1296 625) (** y 9))) (+ (+ (+ (+ (+ (+ (+ (+ (/ -2988 25) (* (/ -2592N -125N) y)) (* (/ 8852 125) (** y 2))) (* (/ -624 25) (** y 3))) (* (/ -1668 125) (** y 4))) (* (/ -88 125) (** y 5))) (* (/ -576 125) (** y 6))) (* (/ -432 125) (** y 7))) (* (/ -432 125) (** y 8))))

```

## License

Copyright © 2014

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

LocalWords:  simulataneous
