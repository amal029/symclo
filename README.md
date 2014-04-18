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

# Example REPL session
symclo.core> (simplify (* a b (** a (/ 1 2))))

((* (** a (/ 3 2)) b))

symclo.core> (simplify (+ a a b))

((+ (* 2 a) b))

symclo.core> (simplify (+ (** (/ 1 2) -2) a b))

((+ (+ 4 a) b))

symclo.core> (simplify (/ (/ a b) a))

((** b -1))


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

## License

Copyright © 2014 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
