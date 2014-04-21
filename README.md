# symclo

A Clojure library for computer algebra systems a la, MAPEL, MAXIMA, MATHEMATICA,...

## Current status

Simplification of rational polynomials is working.

# TODO

Trigonometric simplification.

Polynomial factorization

Simplification of polynomials with imaginary coefficients.

Polynomial integration

Polynomial differentiation (currently differentiation is available in Incanter)

## Usage

(simplify (expr)*)

# Operators:
* '* multiply
* '** exponentiation
* '+ addition
* '- subtraction
* '/ quotient or fraction
* '! factorial

# Example REPL session
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

### Do multiple expressions together
symclo.core> (simplify (/ (/ a b) a) (** (/ 1 x) -2))

((** b -1) (** x 2))

symclo.core> (simplify (* (+ a b) (+ a b)) (* (- a b) (- a b)))

((** (+ a b) 2) (** (+ a (* -1 b)) 2))

symclo.core> (simplify (+ (+ a b) (+ a b)) (+ (- a b) (- a b)))

((+ (* 2 a) (* 2 b)) (+ (* 2 a) (* -2 b)))

symclo.core> (simplify (+ (+ a b) (+ a b)) (+ a b a b))

((+ (* 2 a) (* 2 b)) (+ (* 2 a) (* 2 b)))

### Check equality of two expressions
symclo.core> (= (simplify (+ a b a b)) (simplify (+ (+ a b) (+ a b))))

true

### Trignometric simplification 
#### (currently manual -- you can still use it in style of theorem proving)

symclo.core> (simplify* `(~'+ ~(trig/tr5 '(** (sin x) 2)) (~'** (~'cos ~'x) 2)))

1

symclo.core> (simplify* `(~'+ ~(trig/tr11 '(cos (* 2 a))) ~(trig/tr5 '(** (sin a) 2))))

(** (cos a) 2)

### Check equality of two trignometric expressions
``` clojure
symclo.core> (= 
	     (simplify* (trig/tr4 '(tan (/ %pi 3)))) 
	     (simplify* (list '/ (trig/tr4 '(sin (/ %pi 3))) (trig/tr4 '(cos (/ %pi 3))))))

true
```

## License

Copyright © 2014 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
