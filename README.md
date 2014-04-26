# symclo

A Clojure library for computer algebra systems a la, MAPEL, MAXIMA, MATHEMATICA,...

## Current status

Simplification of rational polynomials

Simplification of trignometric expressions

Expansion of polynomials with rational coefficients

Rationalization of polynomials with rational coefficients

Simple derivates for polynomials 

# TODO

~~Automatic trigonometric simplification algorithm~~

Polynomial factorization

Simplification of polynomials with imaginary coefficients.

Polynomial integration

~~Polynomial differentiation (currently differentiation is available in Incanter)~~

## Usage

* (simplify (expr)*): simplify expression
* (expand (expr)*): expand expression

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
* %pi PI
* %ln natural logarithm

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
symclo.core> (deriv/deriv* 'x 'x)

1


symclo.core> (deriv/deriv* '(+ x 2) 'x)

1

symclo.core> (deriv/deriv* '(* x 2) 'x)

2

symclo.core> (deriv/deriv* '(** x 2) 'x)

(* 2 x)

symclo.core> (deriv/deriv* '(** x (** x 2)) 'x)

(+ (** x (+ 1 (** x 2))) (* (* 2 (%ln x)) (** x (+ 1 (** x 2)))))

symclo.core> (deriv/deriv* '(** (+ x y) 4) 'x)

(* (* 4 (** (+ x y) 3)) (+ 4 (* 4 (%deriv y x))))

symclo.core> (simplify* (deriv/deriv* (expand/expand* '(** (+ x y) 4)) 'x))

(+ (+ (+ (+ (+ (+ (+ (* 4 (** y 3)) (* (* 12 x) (** y 2))) (* (* 4 (%deriv y x)) (** x 3))) (* 4 (** x 3))) (* (* (* (** x 2) 12) (%deriv y x)) y)) (* (* 12 (** x 2)) y)) (* (* (* x 12) (%deriv y x)) (** y 2))) (* (* 4 (%deriv y x)) (** y 3)))

symclo.core> (def a (trig/trig-simplify* '(+ (cos (* 2 a)) (** (sin a) 2))))

#'symclo.core/a

symclo.core> a

(+ (/ 1 2) (* (/ 1 2) (cos (* 2 a))))

symclo.core> (deriv/deriv* a 'a)
(* -1 (sin (* 2 a)))

```

## License

Copyright © 2014

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
