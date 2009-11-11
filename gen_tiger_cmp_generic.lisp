;; tiger-c-cmp.lisp -- generator for the tiger compression function in c
;;
;; Copyright (C) 2009 Mario Xerxes Castelan Castro
;:
;; This file is part of Zirrux 130
;;
;; Zirrux 130 is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Zirrux 130 is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Zirrux 130.  If not, see <http://www.gnu.org/licenses/>.

(defvar *debug* nil)
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
  (format outfile (indent "~a ^= ~a;") c x)
  (format outfile (indent "~a -= ~a1[~a] ^ ~a2[~a] ^ ~a3[~a] ^ ~a4[~a];")
	  a sbox-prefix (nth-byte c 0)
	  sbox-prefix (nth-byte c 2)
	  sbox-prefix (nth-byte c 4)
	  sbox-prefix (nth-byte c 6))
  (format outfile (indent "~a += ~a1[~a] ^ ~a2[~a] ^ ~a3[~a] ^ ~a4[~a];")
	  b sbox-prefix (nth-byte c 1)
	  sbox-prefix (nth-byte c 3)
	  sbox-prefix (nth-byte c 5)
	  sbox-prefix (nth-byte c 7))
  (format outfile (indent "~a *= ~a;") b mul)
  (format outfile (indent "printf(\"round %x ~x: %x %x %x\", ~a, ~a, ~a, ~a)") mul x a b c))

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

(defun tiger-key-schendule (outfile keyvar)
  (tiger-key-schendule-round outfile keyvar 0 "0xA5A5A5A5A5A5A5A5")
  (tiger-key-schendule-round outfile keyvar 3 "((~x1)<<19)")
  (tiger-key-schendule-round outfile keyvar 6 "((~x4)>>23)")
  (tiger-key-schendule-round outfile keyvar 1 "((~x7)<<19)")
  (tiger-key-schendule-round outfile keyvar 4 "((~x2)>>23)")
  (format outfile (indent "x[7] -= x[6] ^ 0x0123456789ABCDEF;")))

(defun tiger-compression (outfile funname)
  (format outfile (indent "void"))
  (format outfile (indent "~a(uint64_t * registers, uint64_t * keys)") funname)
  (format outfile (indent "{"))
  (with-indentation 2
    (format outfile (indent "uint64_t a = registers[0];"))
    (format outfile (indent "uint64_t b = registers[1];"))
    (format outfile (indent "uint64_t b = registers[2];"))
    (tiger-pass outfile "a" "b" "c" 5 "keys" "t")
    (tiger-key-schendule outfile "keys")
    (tiger-pass outfile "c" "a" "b" 7 "keys" "t")
    (tiger-key-schendule outfile "keys")
    (tiger-pass outfile "b" "c" "a" 9 "keys" "t")
    (format outfile (indent "registers[0] ^= a;"))
    (format outfile (indent "registers[1] -= b;"))
    (format outfile (indent "registers[2] += c;")))
  (format outfile (indent "}")))

(when (< 1 (length *posix-argv*))
  (with-open-file (outfile (elt *posix-argv* 1) :direction :output :if-does-not-exist :create)
    ;; TODO Put the GPL notice in the output file
    (format outfile (indent "#include <stdint.h>"))
    (format outfile (indent "#include \"tiger-cmp.h\""))
    (tiger-compression outfile "tiger_cmp"))
  (quit))
