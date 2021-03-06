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
(defvar *indentation* 0)
(defmacro with-indentation (indentation &body forms)
  `(let ((*indentation* (+ ,indentation *indentation*)))
     (declare (special *indentation*))
     ,@forms))

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
  (format outfile (indent "~a += ~a4[~a] ^ ~a3[~a] ^ ~a2[~a] ^ ~a1[~a];")
	  b sbox-prefix (nth-byte c 1)
	  sbox-prefix (nth-byte c 3)
	  sbox-prefix (nth-byte c 5)
	  sbox-prefix (nth-byte c 7))
  (format outfile (indent "~a *= ~a;") b mul)
  (when nil
    (format outfile (indent (format nil "printf(\"%llx ~a: %llx %llx %llx\\n\", ~a, ~a, ~a, ~a);" mul x a b c)))))

(defun tiger-pass (outfile a b c mul keys sbox-prefix)
  (loop for i from 0 to 7
	do (progn
	     (tiger-round outfile a b c (format nil "~a[~a]" keys i) mul sbox-prefix)
	     (rotatef a b c))))

(defun tiger-key-schendule-round (outfile keyvar off sub-xor-const)
  (flet ((w+ (x y) (mod (+ x y) 8)))
    (format outfile (indent "~a[~d] -= ~a[~d] ^ ~a;") keyvar off keyvar (w+ off 7) sub-xor-const)
    (format outfile (indent "~a[~d] ^= ~a[~d];") keyvar (w+ off 1) keyvar off)
    (format outfile (indent "~a[~d] += ~a[~d];") keyvar (w+ off 2) keyvar (w+ off 1))))

(defun tiger-key-schendule (outfile keyvar)
  (tiger-key-schendule-round outfile keyvar 0 "0xA5A5A5A5A5A5A5A5")
  (tiger-key-schendule-round outfile keyvar 3 (format nil "((~~~a[1])<<19)" keyvar))
  (tiger-key-schendule-round outfile keyvar 6 (format nil "((~~~a[4])>>23)" keyvar))
  (tiger-key-schendule-round outfile keyvar 1 (format nil "((~~~a[7])<<19)" keyvar))
  (tiger-key-schendule-round outfile keyvar 4 (format nil "((~~~a[2])>>23)" keyvar))
  (format outfile (indent (format nil "~a[7] -= ~a[6] ^ 0x0123456789ABCDEF;" keyvar keyvar))))

(defun tiger-compression (outfile funname)
  (format outfile (indent "void"))
  (format outfile (indent "~a(uint64_t * registers, uint64_t * keys)") funname)
  (format outfile (indent "{"))
  (with-indentation 2
    (format outfile (indent "uint64_t a = registers[0];"))
    (format outfile (indent "uint64_t b = registers[1];"))
    (format outfile (indent "uint64_t c = registers[2];"))
    (tiger-pass outfile "a" "b" "c" 5 "keys" "t")
    (tiger-key-schendule outfile "keys")
    (tiger-pass outfile "c" "a" "b" 7 "keys" "t")
    (tiger-key-schendule outfile "keys")
    (tiger-pass outfile "b" "c" "a" 9 "keys" "t")
    (format outfile (indent "registers[0] = a ^ registers[0];"))
    (format outfile (indent "registers[1] = b - registers[1];"))
    (format outfile (indent "registers[2] = c + registers[2];")))
  (format outfile (indent "}")))

(print *posix-argv*)

(when t
  (with-open-file (outfile (elt *posix-argv* 1) :direction :output :if-does-not-exist :create)

    (with-indentation 0
      (format outfile "/* tiger_cmp_generic.c
 * Copyright (C) 2009 Mario Castelan Castro
 *
 * This file is part of Zirrux 130
 *
 * Zirrux 130 is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Zirrux 130 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Zirrux 130.  If not, see <http://www.gnu.org/licenses/>.
 */
")
    ;; TODO Put the GPL notice in the output file
      (format outfile (indent "#include <stdint.h>"))
      (format outfile (indent "#include <string.h>"))
      (format outfile (indent "#include \"tiger_sbox.h\""))
      (format outfile (indent "#include \"tiger_cmp.h\""))
      (tiger-compression outfile "ntiger_compress"))))

(quit)
