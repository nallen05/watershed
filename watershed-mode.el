;; watershed-mode.el -- switches between `watershed-lisp-mode' and `watershed-other-mode'
;;                     according to context

;; CREDITS

;; author: 

;;    -Nick Allen <nallen05@gmail.com>

;; watershed-mode.el was created by modifying two-mode-mode.el by

;;    -David N. Welton <davidw@dedasys.com>

;; also crediting

;;    -Marco Pantaleoni <panta@elasticworld.org>
;;    -Janko Heilgeist <janko@heilgeist.com>
;;    -Stefan Schimanski <1stein@gmx.de>
;;
;; watershed-mode inherits two-mode-mode's Apache 2 license:

;; LICENSE

;;    Copyright 1999-2004 The Apache Software Foundation
;;
;;    Licensed under the Apache License, Version 2.0 (the "License");
;;    you may not use this file except in compliance with the License.
;;    You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;    Unless required by applicable law or agreed to in writing, software
;;    distributed under the License is distributed on an "AS IS" BASIS,
;;    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;    See the License for the specific language governing permissions and
;;    limitations under the License.

;; DOCUMENTATION

;; Every buffer starts in WATERSHED-OTHER-MODE (probably HTML-MODE).

;; If the point is inside an S-Expression then the buffer is switched to
;; WATERSHED-LISP-MODE (probably LISP-MODE). So if the buffer contains

;;    <html>
;;     <head><title>Message Page</title>
;;     <body>
;;       <h1>Message<h2>
;;       <p>(he (get-message))</p>
;;     </body>
;;    </html>

;; the buffer will start in WATERSHED-OTHER-MODE then if the point is moved within the
;; expression (he (get-message)), WATERSHED-LISP-MODE will be activated. buffer will
;; be switched back to WATERSHED-OTHER-MODE when the point is moved anywhere else.

;; The lisp operators HTML and XML switch the mode back to WATERSHED-OTHER-MODE within
;; s-expressions. so if the buffer contains


;;    <html>
;;     <head><title>Message Page</title>
;;     <body>
;;       <h1>Messages<h2>
;;       <ol>
;;         (dolist (m (get-message))
;; 	  (html
;;
;; 	     <li>Message: (he m)</li>
;;
;; 	   ))
;;        </li>
;;     </body>
;;    </html>

;; then WATERSHED-LISP-MODE is activated when the point is within

;;    (dolist (m (get-message))
;;      (html...
;;
;;      ))

;; then the buffer is switched back to WATERSHED-OTHER-MODE within the call to HTML

;;      (html
;;
;;         <li>Message: (he m)</li>
;;
;; 	)

;; then WATERSHED-LISP-MODE will be activated again if the point goes within (he m).

;; Use the \ (backslash) character to insert literal "\\", "(", or ")" characters
;; without them being picked up by the parser

;; FAQ

;; Q: UTF-8 BREAKS MY SLIME / EMACS

;; A: try putting this in your .emacs file:

;;       (set-language-environment "UTF-8")
;;       (setq slime-net-coding-system 'utf-8-unix)

;; configure these:
(defvar watershed-lisp-mode '("Lisp Mode" lisp-mode))
(defvar watershed-other-mode '("HTML" html-mode))

;; ----------------

(defvar watershed-update 0)
(defvar watershed-mode-idle-timer nil)
(defvar watershed-bool nil)
(defvar watershed-mode-delay (/ (float 1) (float 8)))

;; Two mode hook
(defvar watershed-mode-hook nil
  "*Hook called by watershed-mode.")
(setq watershed-hook nil)

;; Mode switching hook
(defvar watershed-switch-hook nil
  "*Hook called upon mode switching.")
(setq watershed-switch-hook nil)

(defun watershed-mode-setup ()
  (make-local-hook 'post-command-hook)
  (add-hook 'post-command-hook 'watershed-mode-need-update nil t)
  (make-local-variable 'minor-mode-alist)
  (make-local-variable 'watershed-bool)
  (setq watershed-bool t)
  (when watershed-mode-idle-timer
    (cancel-timer watershed-mode-idle-timer))
  (setq watershed-mode-idle-timer
	(run-with-idle-timer watershed-mode-delay t
			     'watershed-mode-update-mode))
  (or (assq 'watershed-bool minor-mode-alist)
      (setq minor-mode-alist
	    (cons '(watershed-bool " Watershed") minor-mode-alist))))

(defun watershed-mode-need-update ()
  (setq watershed-update 1))

(defun watershed-change-mode (to-mode func)
  (if (string= to-mode mode-name)
      t
    (progn
      (funcall func)
      ;; After the mode was set, we reread the "Local Variables" section.
      ;; We do need this for example in SGML-mode if "sgml-parent-document"
      ;; was set, or otherwise it will be reset to nil when sgml-mode is left.
      (hack-local-variables)
      (watershed-mode-setup)

      (if watershed-switch-hook
	  (run-hooks 'watershed-switch-hook))
      (if (eq font-lock-mode t)
	  (font-lock-fontify-buffer))
      (turn-on-font-lock-if-enabled))))

 (defun watershed.at-escaped-p (&optional pos)
   (let ((b4 (char-before (or pos (point)))))
     (and b4
	  (= b4 92)                          ; escape char
	  (not (watershed.at-escaped-p b4)))))

(defun watershed.at-watershed-form-p ()
  (and (or (posix-looking-at "(html\\W")
	   (posix-looking-at "(xml\\W"))
       (not (watershed.at-escaped-p))))

(defun watershed.in-lisp-p ()
  (let ((depth 0))
    (save-excursion
      (dotimes (n (- (point) (point-min)))
	(backward-char)
	(if (not (watershed.at-escaped-p))
	    (case (char-after)
	      (40 (if (= depth 0)             ; open paren
		      (return (if (not (watershed.at-watershed-form-p))
				  (point)))
		      (decf depth)))
	      (41 (incf depth))))))))	     ; close paren

(defun watershed-mode-update-mode ()
  (when (and watershed-bool watershed-update)
    (setq watershed-update 0)
    (apply 'watershed-change-mode
	   (if (watershed.in-lisp-p)
	       watershed-lisp-mode
	       watershed-other-mode))))

(defun watershed-mode ()
  "Turn on watershed-mode"
  (interactive)
  (funcall (cadr watershed-other-mode))
  (watershed-mode-setup)
  (if watershed-mode-hook
     (run-hooks 'watershed-mode-hook)))

(provide 'watershed-mode)