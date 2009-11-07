;; tiger-c-cmp.lisp -- generator for the tiger compression function in c
;;
;; copyright (c) 2009 mario xerxes castelan castro
;;
;; this file is part of zirrux 130 and is a modified version of
;; tiger.lisp from zirrux 129.
;;
;; zirrux 130 is free software: you can redistribute it and/or modify
;; it under the terms of the gnu general public license as published by
;; the free software foundation, either version 3 of the license, or
;; (at your option) any later version.
;;
;; zirrux 130 is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  see the
;; gnu general public license for more details.
;;
;; you should have received a copy of the gnu general public license
;; along with zirrux 130.  if not, see <http://www.gnu.org/licenses/>.

(require 'split-sequence)


(defmacro with-indentation (indentation &body forms)
  `(let ((*indentation* (+ ,indentation *indentation*)))
     ,@forms))

(defparameter *indentation* 0)
(defconstant +a-reg-initial-state+ "0x0123456789abcdef")
(defconstant +b-reg-initial-state+ "0xfedcba9876543210")
(defconstant +c-reg-initial-state+ "0xf096a5b4c3b2e187")

(defun indent (string)
  (concatenate 'string
	       (make-array *indentation* :initial-element #\space)
	       string
	       #(#\newline)))

;; Regresar el código en C que devuelve el navo byte menos
;; singificativo de una variable en C
(defun nth-byte (var n)
  (if (zerop n)
      (format nil "(~a & 0xFF)" var)
      (format nil "((~a >> ~d) & 0xFF)" var (* 8 n))))

(defun tiger-round (outfile a b c x mul sbox-prefix)
  (format outfile (indent "~a ^= ~a") c x)
  (format outfile (indent "~a -= ~a1[~a] ^ ~a2[~a] ^ ~a3[~a] ^ ~a4[~a]")
	  a sbox-prefix (nth-byte c 0)
	  sbox-prefix (nth-byte c 2)
	  sbox-prefix (nth-byte c 4)
	  sbox-prefix (nth-byte c 6))
  (format outfile (indent "~a += ~a1[~a] ^ ~a2[~a] ^ ~a3[~a] ^ ~a4[~a]")
	  b sbox-prefix (nth-byte c 1)
	  sbox-prefix (nth-byte c 3)
	  sbox-prefix (nth-byte c 5)
	  sbox-prefix (nth-byte c 7))
  (format outfile (indent "~a *= ~a") b mul))

(defun tiger-pass (outfile a b c mul keys sbox-prefix)
  (loop for i from 0 to 7
	do (progn
	     (tiger-round outfile a b c (format nil "~a[~d]" keys i) mul sbox-prefix)
	     (rotatef a b c))))

(defun tiger-key-schendule-round (outfile keyvar off sub-xor-const)
  (flet ((w+ (x y) (mod (+ x y) 8)))
    (format outfile (indent "~a[~d] -= ~a[~d] ^ ~x;") keyvar off keyvar (w+ off 7) sub-xor-const)
    (format outfile (indent "~a[~d] ^= ~a[~d];") keyvar (w+ off 1) keyvar off)
    (format outfile (indent "~a[~d] += ~a[~d];") keyvar (w+ off 2) keyvar (w+ off 1))))

(defun tiger-key-schendule (keyvar)
  (tiger-key-schendule-round keyvar 0 "0xA5A5A5A5A5A5A5A5")
  (tiger-key-schendule-round keyvar 3 "((~x1)<<19)")
  (tiger-key-schendule-round keyvar 6 "((~x4)>>23)")
  (tiger-key-schendule-round keyvar 1 "((~x7)<<19)")
  (tiger-key-schendule-round keyvar 4 "((~x2)>>23)")
  (format *standard-output* (indent "x[7] -= x[6] ^ 0x0123456789ABCDEF;")))
