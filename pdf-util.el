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

