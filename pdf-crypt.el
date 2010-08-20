(require 'arc4)

(setq pdf-crypto-padding 
      "\x28\xbf\x4e\x5e\x4e\x75\x8a\x41\x64\x00\x4e\x56\xff\xfa\x01\x08\x2e\x2e\x00\xb6\xd0\x68\x3e\x80\x2f\x0c\xa9\xfe\x64\x53\x69\x7a")

(defun pdf-num-to-bytes (n)
  (let (out)
    (dotimes (i 4) 
      (push (logand n 255) out)
      (setq n (ash n -8))) 
    (string-make-unibyte (concat (nreverse out)))))

(defun pdf-dehexify (s)
  (string-make-unibyte
   (replace-regexp-in-string
    "[ \t\r\n\x00\x0c]*[0-9A-Fa-f]\\{2\\}[ \t\r\n\x00\x0c]*"
    (lambda (m)
      (char-to-string
       (string-to-number m 16)))
    s t t)))

(defun pdf-getkey (doc)
  (let* ((trailer (pdf-doc-trailer doc))
	 (crypto (pdf-dref trailer '/Encrypt))
	 (cksum (md5 (string-make-unibyte
		      (concat pdf-crypto-padding
			      (pdf-dref crypto '/O)
			      (pdf-num-to-bytes (pdf-dref crypto '/P))
			      (pdf-dehexify (cadr (pdf-ref* trailer '/ID 0)))))
		     nil nil 'binary)))
    (dotimes (i 50 (pdf-dehexify cksum))
      (setq cksum (pdf-dehexify cksum)
	    cksum (md5 cksum)))))

(defun pdf-decrypt (key objref data)
  (let* ((onum (substring (pdf-num-to-bytes (cadr objref)) 0 3))
	 (gnum (substring (pdf-num-to-bytes (caddr objref)) 0 2))
	 (key (string-make-unibyte
		    (concat key onum gnum)))
	 (keylen (length key))
	 (key (pdf-dehexify (md5 key))))
    (rc4 (substring key 0 (min 16 (length key)))
	 (copy-seq data))))

(defun pdf-rehexify (s)
  (apply 'concat
	 (lc
	  (format "\\x%02x" x) x
	  (append s nil))))
