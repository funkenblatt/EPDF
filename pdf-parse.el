;;; pdf-parse.el --- pdf parsing routines

;; Copyright (C) 2010 Jason Feng

;; Author: Jason Feng <jfeng1985@gmail.com>
;; Maintainer: Jason Feng <jfeng1985@gmail.com>
;; Created: 15 Aug 2010
;; Keywords: docview pdf outline

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; This code handles parsing of basic PDF data structures.  For doing
;; more useful things, see pdf-util.el.

;; Main entry point for usage is (`pdf-init'), called with some PDF
;; file as the current buffer.  The return value is a pdf-doc struct,
;; with which various useful things can in theory be done.

;; This code is intended to be used with doc-view.  If doc-view doesn't
;; exist, this code will not necessarily fail, but certainly won't be
;; very useful.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'pdf-crypt)

(defstruct (pdf-doc)
  (xrefs)
  (catalog)
  (objs)
  (buf)
  (names)
  (trailer)
  key)

(defstruct (pdf-obj)
  oid doc)

(defstruct (pdf-dict (:include pdf-obj))
  alist)

(defstruct (pdf-array (:include pdf-obj))
  v)

(defstruct (pdf-str (:include pdf-obj))
  s decrypted)

(defmacro pdf-chr (s) 
  "Converts a string literal into an integer literal.  Mainly used 
because ?( screws up emacs's paren matching."
  (aref s 0))

(defun pdf-read-n (n)
  "Returns a compiled function object that reads in N characters
and returns whatever that was.  Usually used for snarfing symbols
like true, false, and null, etc."
  (byte-compile
   `(lambda (doc &optional oid)
      (prog1
	  (read (buffer-substring (point) (+ (point) ,n)))
	(forward-char ,n)))))

(setq pdf-token-handlers
  (mapcar
   (lambda (x) (cons (aref (car x) 0)
		     (cdr x)))
   `(("t" . ,(pdf-read-n 4))
     ("f" . ,(pdf-read-n 5))
     ("R" . ,(pdf-read-n 1))
     ("(" . pdf-readstr)
     ("/" . pdf-readname)
     ("[" . pdf-readarray)
     ("n" . ,(pdf-read-n 4))
     ("%" . pdf-comment-skip))))

(defun pdf-read (doc &optional oid)
  "Read whatever PDF object may be at point in the current buffer."
  (pdf-skipws)
  (let* ((standard-input (current-buffer))
	 (c (char-after (point)))
	 (handler (assq c pdf-token-handlers)))
    (cond
     (handler (funcall (cdr handler) doc oid))
     ((eq c ?<) (if (eq (char-after (+ (point) 1)) ?<)
		    (pdf-readdict doc oid)
		  (pdf-readhex doc oid)))
     (t (pdf-readnum doc)))))

(defun pdf-readnum (doc &optional oid)
  (read (progn (re-search-forward "[+-]?[0-9.]+")
	       (match-string 0))))

(defun pdf-readname (doc &optional oid)
  (re-search-forward
   (rx "/" (* (not (any "%/()[]<>{} \x00\t\r\n\x0c")))))
  (intern (match-string 0)))

(defun pdf-readstr (doc &optional oid)
  (forward-char 1)
  (let ((start (point))
	(depth 1))
    (while (> depth 0)
      (re-search-forward
       (rx (or (: "\\" (or (repeat 3 (any (?0 . ?9)))
			   (any "\nntbrf()\\")))
	       (any "()"))))
      (unless (= (aref (match-string 0) 0) ?\\)
	(incf depth
	      (if (equal (match-string 0) "(") 1 -1))))
    (make-pdf-str
     :s (replace-regexp-in-string
	 (rx "\\" (or (repeat 3 (any (?0 . ?9)))
		      (any "\nntrbf()\\")))
	 (lambda (m)
	   (if (string= m "\\\n")
	       ""
	     (read (concat "\"" m "\""))))
	 (buffer-substring start (- (point) 1))
	 nil t)
     :oid oid :doc doc
     :decrypted (not (pdf-doc-key doc)))))

(defun pdf-skipws ()
  (skip-chars-forward
   " \x00\t\r\n\x0c")
  (point))

(defun pdf-grab-til-char (doc c &optional oid)
  "Keep reading in PDF objects until the character C is encountered.
Also handles indirect object references."
  (let (out)
    (while (/= (char-after (point)) c)
      (push (pdf-read doc oid) out)
      (when (eq (car out) 'R)
	(pop out)
	(push (cons 'R (nreverse (list (pop out) (pop out))))
	      out))
      (pdf-skipws))
    out))

(defun pdf-readarray (doc &optional oid)
  (forward-char 1)
  (pdf-skipws)
  (let ((out (pdf-grab-til-char doc (pdf-chr "]") oid)))
    (forward-char)
    (make-pdf-array :v (apply 'vector (nreverse out))
		    :doc doc
		    :oid oid)))

(defun pdf-arraylen (a)
  (length (pdf-array-v a)))

(defun pdf-readdict (doc &optional oid)
  (forward-char 2)
  (pdf-skipws)
  (let ((tmp (pdf-grab-til-char doc (pdf-chr ">") oid))
	out)
    (while tmp
      (push (cons (cadr tmp) (car tmp)) out)
      (setq tmp (cddr tmp)))
    (forward-char 2)
    (make-pdf-dict :oid oid :alist out :doc doc)))

(defun pdf-readhex (doc &optional oid)
  (forward-char)
  (let ((start (point))
	(end (- (re-search-forward ">") 1)))
    (list 'hex (buffer-substring start end))))

(defun pdf-dref (dict key &optional noderef noread)
  "Retreive an item from a PDF dict.  If the value stored
there is an indirect reference, dereference it, unless
NODEREF is non-nil.  NOREAD is forwarded to `pdf-getxref',
which see."
  (let ((out (cdr (assq key (pdf-dict-alist dict)))))
    (if (and (pdf-objref-p out)
	     (not noderef))
	(pdf-getxref (pdf-dict-doc dict) out noread)
      out)))

(defun pdf-objref-p (x)
  "Is this an indirect object reference?"
  (and (consp x)
       (eq (car x) 'R)))

(defun pdf-aref (arr ix &optional noderef noread)
  "Kind of like `pdf-dref', but for arrays."
  (let ((out (aref (pdf-array-v arr) ix)))
    (if (and (pdf-objref-p out)
	     (not noderef))
	(pdf-getxref (pdf-array-doc arr) out noread)
      out)))

(defun pdf-ref* (obj &rest refs)
  "Descends a chain of links in a PDF datastructure.  For example,

\t(pdf-refchain x '/Foo '/Bar 3)

is equivalent to 

\t(pdf-aref (pdf-dref (pdf-dref x '/Foo) '/Bar) 3)"
  (dolist (i refs obj)
    (if (integerp i)
	(setq obj (pdf-aref obj i))
      (setq obj (pdf-dref obj i)))))

(defun pdf-xrefs (doc)
  "Retreive the document's cross reference tables."
  (read (current-buffer)) 		;skip "xref" token
  (let ((segments `((,(pdf-read doc) ,(pdf-read doc) ,(pdf-skipws))))
	trailer)
    (catch 'break
      (while t
	(forward-char (* (cadar segments) 20))
	(if (eq (char-after (point)) ?t)
	    (throw 'break segments)
	  (push (list (pdf-read doc) (pdf-read doc)
		      (pdf-skipws))
		segments))))
    (read (current-buffer)) 		;skip "trailer" token
    (setq trailer (pdf-read doc))
    (when (pdf-dref trailer '/Prev)
      (goto-char (pdf-dref trailer '/Prev))
      (setq segments (append segments (aref (pdf-xrefs doc) 1))))
    (vector trailer segments)))

(defun pdf-getxref (doc objref &optional noread)
  "Retreive indirect object reference OBJREF for document
DOC.  If NOREAD is non-nil, return the buffer offset of the
object instead of actually reading it."
  (when (not (pdf-doc-objs doc))
    (setf (pdf-doc-objs doc) (make-hash-table :test 'equal)))
  (let ((xrefs (pdf-doc-xrefs doc))
	(objnum (cadr objref))
	(obj (gethash objref (pdf-doc-objs doc)))
	val)
    (if (and obj (not noread))
	obj
      (with-current-buffer (pdf-doc-buf doc)
	(while (and xrefs
		    (not (and (>= objnum (caar xrefs))
			      (< objnum (+ (caar xrefs)
					   (cadar xrefs))))))
	  (pop xrefs))
	(if (not xrefs)
	    'null
	  (setq xrefs (car xrefs))
	  (save-excursion
	    (goto-char (caddr xrefs))
	    (forward-char (* (- objnum (car xrefs)) 20))
	    (goto-char (+ (point-min) (pdf-read doc)))
	    (search-forward "obj")
	    (if noread
		(point)
	      (setq val (pdf-read doc objref))
	      (puthash objref val (pdf-doc-objs doc))
	      val)))))))

(defun pdf-init ()
  "Creates a pdf-doc structure.  Initializes the cross-reference table
slot and finds the document catalog, which is required for doing just 
about anything else with the document."
  (let ((out (make-pdf-doc :buf (current-buffer))))
    (save-excursion
      (goto-char (point-max))
      (search-backward "startxref" 1024)
      (goto-char (match-end 0))
      (goto-char (+ (pdf-read out) (point-min)))
      (let* ((junk (pdf-xrefs out))
	     (trailer (aref junk 0)))
	(setf (pdf-doc-trailer out) trailer
	      (pdf-doc-xrefs out) (aref junk 1)
	      (pdf-doc-catalog out) (pdf-dref trailer '/Root)
	      (pdf-doc-key out) (and (pdf-dref trailer '/Encrypt)
				     (pdf-getkey out))))
      out)))

(provide 'pdf-parse)

;;; pdf-parse.el ends here
