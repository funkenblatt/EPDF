;;; pdf-util.el --- pdf parsing routines

;; Copyright (C) 2010 Jason Feng

;; Author: Jason Feng <jfeng1985@gmail.com>
;; Maintainer: Jason Feng <jfeng1985@gmail.com>
;; Created: 16 Aug 2010
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
;; This code does somewhat useful things with PDF
;; documents.  Namely, `pdf-outlines' extracts the document
;; outlines of a PDF file and dumps them into a buffer.
;; `pdf-outlines-goto' retreives an outline entry from the text
;; properties at point, and jumps to that page in the doc-view
;; buffer from which the outline was extracted.

;; This code defines no key bindings, but the main entry
;; point here is (`pdf-outlines' <pdf-doc>), where <pdf-doc>
;; is a pdf-doc structure that has most likely been retreived
;; using `pdf-init' defined in pdf-parse.el.  Recommended usage
;; is to add a binding to doc-view-mode-map or something.

;; This code is intended to be used with doc-view.  If doc-view doesn't
;; exist, this code will not necessarily fail, but certainly won't be
;; very useful.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'outline)
(require 'pdf-parse)

(defun pdf-ref* (obj &rest refs)
  "Descends a chain of links in a PDF datastructure.  For example,

\t(pdf-refchain x '/Foo '/Bar 3)

is equivalent to 

\t(pdf-aref (pdf-dref (pdf-dref x '/Foo) '/Bar) 3)"
  (dolist (i refs obj)
    (if (integerp i)
	(setq obj (pdf-aref obj i))
      (setq obj (pdf-dref obj i)))))

(defun pdf-dump (&rest args)
  (mapc (lambda (x) (princ x (current-buffer))) args))

(defun pdf-outlines (doc)
  (if (pdf-dref (pdf-doc-catalog doc) '/Outlines)
      (let* ((buf (get-buffer-create "*Outlines*")))
	(with-current-buffer buf
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (pdf-dump-outlines doc)
	  (goto-char (point-min))
	  (pdf-outline-mode)
	  (call-interactively 'hide-sublevels)      
	  (setq buffer-read-only t))
	(pop-to-buffer buf))))

(defun pdf-map-outlines (proc outlines &optional depth)
  (if (not depth) (setq depth 0))
  (while outlines
    (funcall proc outlines depth)
    (when (pdf-dref outlines '/First)
      (pdf-map-outlines proc (pdf-dref outlines '/First) (+ depth 1)))
    (setq outlines (pdf-dref outlines '/Next))))

(defun pdf-dump-outlines (doc)
  (pdf-map-outlines
   (lambda (x depth)
     (when (pdf-dref x '/Title)
       (dotimes (i depth) (pdf-dump "*"))
       (dotimes (i depth) (pdf-dump " "))
       (let ((title (pdf-dref x '/Title)))
	 (if (and (> (length title) 1)
		  (string= (substring title 0 2)
		      "\xfe\xff"))	;Check for byte-order mark
	     (setq title (decode-coding-string title 'utf-16)))
	 (pdf-dump title))
       (add-text-properties (line-beginning-position)
			    (line-end-position)
			    (list 'outline-obj x))
       (pdf-dump "\n")))
   (pdf-dref (pdf-doc-catalog doc) '/Outlines)))

(defun pdf-lookup-dest (doc dest)
  (when (not (pdf-doc-names doc))
    (setf (pdf-doc-names doc) (make-hash-table :test 'equal)))
  (let ((out (gethash dest (pdf-doc-names doc))))
    (if out out
      (setq out
	    (pdf-nametree-ref 
	     (pdf-dref (pdf-dref (pdf-doc-catalog doc) '/Names)
		       '/Dests)
	     dest))
      (puthash dest out (pdf-doc-names doc)))))

(defun pdf-arrayfind (proc array &optional deref)
  (catch 'break
    (dotimes (i (pdf-arraylen array))
      (if (funcall proc (pdf-aref array i (not deref)))
	  (throw 'break i)))))

(defun string-between (s l u)
  (or
   (and (string< s u)
	(not (string< s l)))
   (string= s l)
   (string= s u)))

(defun pdf-nametree-ref (ntree name)
  "Lookup a name in a name tree"
  (let (kids limits ubound lbound ix)
    (catch 'break
      (while t
	(if (setq kids (pdf-dref ntree '/Kids))
	    (if (setq ix
		      (pdf-arrayfind
		       (lambda (kid)
			 (setq limits (pdf-dref kid '/Limits)
			       lbound (pdf-aref limits 0)
			       ubound (pdf-aref limits 1))
			 (string-between name lbound ubound))
		       kids t))
		(setq ntree (pdf-aref kids ix))
	      (throw 'break nil))
	  (setq kids (pdf-dref ntree '/Names)
		ix (pdf-arrayfind (lambda (kid)
				    (if (stringp kid)
					(string= kid name)))
				  kids))
	  (throw 'break (pdf-aref kids (+ ix 1))))))))

(defun pdf-outline-page (outline)
  "Find the page number that a particular outline entry
refers to."
  (let ((dest (or (pdf-dref outline '/Dest)
		  (pdf-ref* outline '/A '/D)))
	doc)
    (when (stringp dest)
      (setq dest 
	    (pdf-lookup-dest
	     (pdf-dict-doc outline)
	     dest)))
    (cond
     ((pdf-array-p dest) (setq doc (pdf-array-doc dest)
			       dest (pdf-aref dest 0 t)))
     ((pdf-dict-p dest) (setq doc (pdf-dict-doc dest)
			      dest (pdf-aref
				    (pdf-dref dest '/D) 0 t))))
    (pdf-find-pageno doc dest)))

(defun pdf-find-pageno (doc page)
  "Takes document DOC and indirect reference PAGE, and finds
the numerical page index of PAGE."
  (let* ((index 0)
	 (node (pdf-getxref doc page))
	 tmp)
    (while node
      (if (eq (pdf-dref node '/Type) '/Page)
	  (setq index (pdf-arrayfind
		       (lambda (kid)
			 (equal kid page))
		       (pdf-ref* node '/Parent '/Kids))
		node (pdf-dref node '/Parent))
	(when (pdf-dref node '/Parent)
	  (pdf-arrayfind
	   (lambda (kid)
	     (unless (eq kid node)
	       (incf index (pdf-dref kid '/Count)))
	     (eq kid node))
	   (pdf-ref* node '/Parent '/Kids)
	   t))
	(setq node (pdf-dref node '/Parent))))
    (+ index 1)))

(defun pdf-outline-goto ()
  "Looks up the outline object property at point, and goes
to whatever page it happens to be linked to."
  (interactive)
  (let* ((o (get-text-property (point) 'outline-obj))
	 (doc (pdf-dict-doc o))
	 (buf (pdf-doc-buf doc)))
    (pop-to-buffer buf)
    (doc-view-goto-page
     (pdf-outline-page o))))

(defvar pdf-outline-mode-map
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km outline-mode-map)
    (define-key km (kbd "RET") 'pdf-outline-goto)
    km)
  "Keymap for pdf-outline viewer.")

(define-derived-mode
  pdf-outline-mode
  outline-mode
  "PDF Outline"
  "PDF Outline mode.  Uses the same navigation shortcuts as
`outline-mode', but also lets you jump to pages within a PDF
document that has been opened with docview.")

(provide 'pdf-util)

;;; pdf-util.el ends here
