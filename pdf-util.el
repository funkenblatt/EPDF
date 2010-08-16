(require 'pdf-parse)

(defun pdf-dump (&rest args)
  (mapc (lambda (x) (princ x (current-buffer))) args))

(defun pdf-outlines (doc)
  (let* ((buf (get-buffer-create "*Outlines*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (pdf-dump-outlines doc)
      (goto-char (point-min))
      (outline-mode)
      (call-interactively 'hide-sublevels)      
      (setq buffer-read-only t))
    (pop-to-buffer buf)))

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
       (pdf-dump (pdf-dref x '/Title))
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

(defun pdf-arrayfind (proc array)
  (catch 'break
    (dotimes (i (pdf-arraylen array))
      (if (funcall proc (pdf-aref array i))
	  (throw 'break i)))))

(defun string-between (s l u)
  (or
   (and (string< s u)
	(not (string< s l)))
   (string= s l)
   (string= s u)))

(defun pdf-nametree-ref (ntree name)
  "Dereference a named destination."
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
		       kids))
		(setq ntree (pdf-aref kids ix))
	      (throw 'break nil))
	  (setq kids (pdf-dref ntree '/Names))
	  (setq ix (pdf-arrayfind (lambda (kid)
				    (if (stringp kid)
					(string= kid name)))
				  kids))
	  (throw 'break (pdf-aref kids (+ ix 1))))))))
