;; RC4 Implementation copied from http://nullprogram.com/blog/2009/04/26/.
;; Modified ever so slightly by Jason Feng.

;; Copyright (c) 2009 Christopher Wellons <mosquitopsu@gmail.com>

;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(defun rc4 (key str)
  "RC4 encrypts/decrypts STR using KEY."
  (let ((state (make-vector 256 0))
	(i 0) (j 0) keybyte)
    (dotimes (x 256) (aset state x x))
    (rc4-key-sched state key)
    (dotimes (x (length str) str)
      (setq i (mod (1+ i) 256)
	    j (mod (+ j (aref state i)) 256))
      (rotatef (aref state i) (aref state j))
      (setf keybyte (aref state (mod (+ (aref state i)
					(aref state j)) 256))
	    (aref str x) (logxor (aref str x) keybyte)))))

(defun rc4-key-sched (rc4-state key)
  "Arcfour key-scheduler: initialize state from key."
  (let ((j 0) i)
    (dotimes (i 256 rc4-state)
      (setq j (% (+ j 
                    (aref rc4-state i) 
                    (aref key (% i (length key)))) 256))
      (rotatef (aref rc4-state i) (aref rc4-state j)))))

(provide 'arc4)
