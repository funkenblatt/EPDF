(require 'arc4)

(setq pdf-crypto-padding 
      "\x28\xbf\x4e\x5e\x4e\x75\x8a\x41\x64\x00\x4e\x56\xff\xfa\x01\x08\x2e\x2e\x00\xb6\xd0\x68\x3e\x80\x2f\x0c\xa9\xfe\x64\x53\x69\x7a")

(defun pdf-num-to-bytes (n)
  (let (out)
    (dotimes (i 4) 
      (push (logand n 255) out)
      (setq n (ash n -8))) 
    (string-make-unibyte (concat (nreverse out)))))

(rdump (pdf-aref (pdf-dref (pdf-doc-trailer doc) '/ID) 0))
(hex "C2136E30A2A8EFE42AF0042296CEF9C7")

(defun pdf-dehexify (s)
  (string-make-unibyte
   (replace-regexp-in-string
    "[0-9A-Fa-f]\\{2\\}"
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

(let: cksum (md5
	     (string-make-unibyte
	      (concat pdf-crypto-padding
		      (pdf-dref cryptowad '/O)
		      (pdf-num-to-bytes (pdf-dref cryptowad '/P))
		      (pdf-dehexify (cadr (pdf-aref (pdf-dref (pdf-doc-trailer doc) '/ID) 0)))))
	     nil nil 'binary) in
  (dotimes (i 50)
    (setq cksum (pdf-dehexify cksum)
	  cksum (md5 cksum)))
  (pdf-dehexify cksum))

(defun pdf-rehexify (s)
  (apply 'concat
	 (lc
	  (format "\\x%02x" x) x
	  (append s nil))))

(dump
(pdf-rehexify (pdf-dref cryptowad '/O)))
"\x8c\xcd\xb6\x8a\x9c\xd1\x78\x82\x43\x72\x9a\xcc\xc8\x77\xb1\x70\x77\x78\xbb\x22\xb2\x0a\xce\x49\xb8\xd6\xc3\x4c\x3d\x46\x09\x7d"

(dump
(pdf-rehexify "\302n0\242\250\357\344*\360\"\226\316\371\307"))

"\xc2\x13\x6e\x30\xa2\xa8\xef\xe4\x2a\xf0\x04\x22\x96\xce\xf9\xc7"


