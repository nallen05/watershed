;
; http://github.com/nallen05/watershed
;

(defpackage :watershed
  (:use :cl :cl-terrace-util)
  (:export ; template api
	   :*html-output*
	   :raw
	   :he
	   :he-to-string
	   :ue
	   :ue-to-string
	   :html
	   :xml

	   ; URL decoding
	   :ud

	   ; an internal function exported for "superior-watershed-mode.el"
	   :prepare-watershed-template-string

	   ; restart offered within generated functions
	   :dump-watershed-buffer

	   ; using
	   :compile-watershed-template))

(defpackage :watershed-user
  (:use :cl :watershed))

(in-package :watershed)

; utils

(defun .in-package-form-p (form)
  "if `FORM' is a list that looks like (in-package <package-designator>) then returns <package-designator>, otherwise returns NIL"
  (and (listp form)
       (null (cddr form))
       (eql (first form) 'in-package)
       (second form)))

(defun .read-from-string-understanding-in-package-forms (string)
  "like READ-FROM-STRING but returns a list of all forms in the string, not just the first form. understands toplevel IN-PACKAGE forms while reading, but discards them after they are read"
  (let (forms
	(done (gensym "done"))
	(*package* *package*))
    (with-input-from-string (in string)
      (do ((form #1=(read in nil done) #1#))
	  ((eql form done) (nreverse forms))
	(if (.in-package-form-p form)
	    (setq *package* (find-package (.in-package-form-p form)))
	    (push form forms))))))

; template api

(defvar *html-output*)

(defun raw (thing)
  "PRINC's `THING' to *HTML-OUTPUT*"
  (princ thing *html-output*))

(define-compiler-macro raw (&whole form thing)
  (logv:format-log "danger! writing raw, non html-escaped form ~S to *html-output*" thing)
  form)


(defun he-to-string (thing)
  "PRINC's `THING' to a string then html-escapes it"
  (cl-terrace-util:html-escape thing (princ-to-string thing)))

(defun he (thing)
  "PRINC's `THING' to *HTML-OUTPUT*, html-escaping it on the way out"
  ;; this function was created by modifying HUNCHENTOOT:ESCAPE-FOR-HTML
  (write-string (he-to-string thing) *html-output*))

(defun ue-to-string (thing)
  "PRINC's `THING' to a string then url-encodes it. assumes UTF-8 so be careful if seen around HUNCHENTOOT:URL-DECODE, which probably assumes LATIN-1 by default. see HUCHENTOOT:*DEFAULT-EXTERNAL-FORAT*"
  (cl-terrace-util:url-encode (princ-to-string thing)))

(defun ue (thing)
  "PRINC's `THING' to *HTML-OUTPUT*, url-encoding it on the way out. assumes UTF-8 so be careful if seen around HUNCHENTOOT:URL-DECODE, which probably assumes LATIN-1 by default. see HUCHENTOOT:*DEFAULT-EXTERNAL-FORAT*"
  (write-string (ue-to-string thing) *html-output*))

(defmacro html (&rest html)
  "special operator used in templates to switch the parser back into HTML mode. the same as WATERSHED:XML"
  `(progn 
     ,@(mapcar (f_ (if (stringp _)
		       `(write-string ,_ *html-output*)
		       _))
	       html)))

(defmacro xml (&rest xml)
  "special operator used in templates to switch the parser back into XML mode. the same as WATERSHED:HTML"
  `(html ,@xml))

; URL decoding

(defun ud (string)
  "returns the a new url-decoded version of `STRING'. assumes UTF-8 so be careful if seen around HUNCHENTOOT:URL-ENCODE, which probably assumes LATIN-1 by default. see HUCHENTOOT:*DEFAULT-EXTERNAL-FORAT*"
  (cl-terrace-util:url-decode string))

; internal fn exported for SLIME-enabled Emacs mode

(defun prepare-watershed-template-string (string)
  "turns unescaped sightings of (html ...illegal...) into (html ...legal-strings-n-sexps...) in a string. also does (xml ..)"
  (do ((n 0 (incf n))
       (stop (length string))
       (buffer (make-string-output-stream))
       (stack (list :start))                           ; :START ~ non-lisp mode
                                                       ; :NESTED   ~ in a call to html or xml (in non-lisp mode inside lisp mode)
                                                       ;  or a number N  ~ lisp mode (N is the paren depth of the lisp state)
       (seen-non-lisp-char?))
      ((>= n stop) (get-output-stream-string buffer))
    (labels ((output (x)
	       (write-string (string x) buffer))
	     (char-escaped? ()
	       (and (> n 0)
		    (char= (char string (1- n)) #\\)))
	     (at-special-op? (op)
	       (let ((op-open  (format nil "(~A" op)))
		 (and (eql (mismatch string op-open :start1 n)
			 (+ (length op-open) n))
		    (>= (length string) (+ n (length op-open)))
		    (let ((% (char string (+ n (length op-open)))))
		      (or (char= % #\space)
			  (char= % #\newline)
			  (char= % #\tab)
			  (char= % #\return)))
		    (not (char-escaped?)))))
	     (enter-op (op)
	       (let ((op-open (format nil "(~A" op)))
		 (if seen-non-lisp-char?
		     (output #\"))
		 (output op-open)
		 (output " \"")
		 (incf n (length op-open))
		 (push :nested stack)
		 (setf seen-non-lisp-char? t)))
	     (dispatch-open-paren ()
	       (cond ((at-special-op? "html") (enter-op "html"))
		     ((at-special-op? "xml")  (enter-op "xml"))
		     (t  	             (enter-lisp))))
	     (enter-lisp ()
	       (if seen-non-lisp-char?
		   (output #\"))
	       (setf seen-non-lisp-char? nil)
	       (output #\()
	       (push 0 stack)))
      (let ((c (char string n)))
	(if (char-escaped?)
	    (output c)           ; danger. is or is not non-lisp-char?
	    (case (first stack)
	      (:start (case c
			 (#\(       (dispatch-open-paren))
			 (otherwise (output c))))
	      (:nested (case c
			 (#\( (dispatch-open-paren))
			 (#\) (if seen-non-lisp-char?
				  (output "\")")
				  (output ")"))
			      (setf seen-non-lisp-char? nil)
			      (pop stack))
			 (#\" (setf seen-non-lisp-char? t)
			      (output "\\\""))
			 (otherwise (if (not seen-non-lisp-char?)
					(output #\"))
				    (setf seen-non-lisp-char? t)
				    (output c))))
	      (otherwise (case c
			   (#\( (dispatch-open-paren))
			   (#\) (output ")")
				(when (< (decf (first stack)) 0) ;!
				  (pop stack)))
			   (otherwise (output c))))))))))

; viewing / debugging lisp source generated by templates

(defun .string->lisp-src-no-buffer (string)
  "returns the lisp source of the function that would be generated by STRING->FUNCTION if it were called with (`STRING' :TYPE :LISP-NO-BUFFER)"
  `(f0 (with-simple-restart (dump-watershed-buffer "Abort evaluating watershed function, returning NIL")
	 ,@(.read-from-string-understanding-in-package-forms string))))

(defun .string->lisp-src (string)
  "returns the lisp source of the function that would be generated by STRING->FUNCTION if it were called with (`STRING' :TYPE :LISP)"
  `(f0 (with-output-to-string (*html-output*)
	 (with-simple-restart (dump-watershed-buffer "Send all the HTML generated so far to the client's browser")
	   ,@(.read-from-string-understanding-in-package-forms string)))))

(defun .string->template-src (string)
  "returns the lisp source of the function that would be generated by STRING->FUNCTION if it were called with (`STRING' :TYPE :TEMPLATE)"
  `(f0 (with-output-to-string (*html-output*)
	 (with-simple-restart (dump-watershed-buffer "Send all the HTML generated so far to the client's browser")
	   (html
	    ,@(let ((% (prepare-watershed-template-string (format nil "(html ~A)" string))))
		   (remove-if 'zerop
			      (.read-from-string-understanding-in-package-forms (subseq % (length "(html ") (1- (length %))))
			      :key 'length)))))))

(defun .check-utf-8 (enctype)
  (assert (eql enctype :utf-8) () "watershed does not currently support encodings other than UTF-8"))

(defun .file->src (path &key (type :template) (enctype :utf-8))
  "returns the lisp source code of the function that would be generated by FILE->FUNCTION if it were called with the same arguments"
  (.check-utf-8 enctype)
  (let ((string (cl-terrace-util:slurp-utf-8-file path)))
    (case type
      (:lisp-no-buffer (.string->lisp-src-no-buffer string))
      (:lisp           (.string->lisp-src string))
      (:template       (.string->template-src string))
      (otherwise       (error "the function ~S does not understand the argument to type ~S" 'string->src type)))))

; using

(defun compile-watershed-template (path &key (type :template) (enctype :utf-8) (compile t) to-bytes)
  "returns a function of 0 arguments.

   -if `TYPE' is eql to :LISP-NO-BUFFER then `PATH' is assumed to contain lisp code which is executed and the values of the last form returned.
   -if `TYPE' is eql to :LISP then `PATH' is assumed to contain lisp code which is executed with the variable *HTML-OUPUT* bound to a string-output-stream. the contents of this buffer are returned by the function.
   -if `TYPE' is eql to :TEMPLATE, then `PATH' is assumed to be a watershed template. The function returns the a string of all the HTML/XML in the template and everything written to *HTML-OUTPUT* according to watershed semantics."
  (if compile
      (values (compile nil (compile-watershed-template path :type type :enctype enctype)))
      (if to-bytes
	  (let ((f (compile-watershed-template path :type type :enctype enctype)))
	    (f0 (trivial-utf-8:string-to-utf-8-bytes (funcall f))))
	  (.file->src path :type type :enctype enctype))))