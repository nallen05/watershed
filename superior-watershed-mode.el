;;  superior-watershed-mode.el -- SLIME-enabled watershed-mode.el

; todo: -compilation-success-hooks (cl and el)

;; CREDITS

;; author:

;;    -Nick Allen <nallen05@gmail.com>

;;  LICENSE

;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;     
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;     
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.

;; DOCUMENTATION

;; superior-watershed-mode provides the following functions

;;    WATERSHED-COMPILE-TEMPLATE-BUFFER ~ SLIME-compiles a watershed template

;;    WATERSHED-COMPILE-LISP-BUFFER ~ SLIME-compiles a buffer (without creating a .fasl)

;; the following key bindings are provided within superior-watershed-mode

;;    \C-c\C-k    watershed-compile-template-buffer
;;    \M-p        slime-previous-note
;;    \M-n        slime-next-note
;;    \C-c\C-z    slime-switch-to-output-buffer
;;    \C-c~       slime-sync-package-and-default-directory
;;    \C-c\M-c    slime-remove-notes

;; superior-watershed-mode.el uses watershed-mode.el so make sure to read it's
;; documentation as well!

;; FAQ

;; Q: UTF-8 BREAKS MY SLIME / EMACS

;; A: try putting this in your .emacs file:

;;       (set-language-environment "UTF-8")
;;       (setq slime-net-coding-system 'utf-8-unix)

;; Q: COMPILING IS SO SLOW!!

;; A: I know. will try to speed it up in the future.

;; Q: \C-c\M-c (SLIME REMOVE NOTES) DOESN'T WORK WHILE IN HTML MODE

;; A: I know. I'm not sure what's wrong. I'll look at it later.

; Dependencies and setup

(require :watershed-mode)
(require :slime)

; superior-watershed-mode

(defvar superior-watershed-bool nil)

(define-minor-mode superior-watershed-mode "SLIME-enabled watershed-mode"
   :keymap '(("\M-p" . slime-previous-note)
	     ("\M-n" . slime-next-note))
   (unless superior-watershed-bool
       (add-hook 'watershed-switch-hook 'superior-watershed-mode)
       (or (assq 'watershed-bool minor-mode-alist)
	   (setq minor-mode-alist
		 (cons '(watershed-bool " Superior-Watershed") minor-mode-alist)))
       (watershed-mode)
       (make-local-variable 'watershed-bool)
       (setq superior-watershed-bool t)
       (superior-watershed-mode)))

(define-key superior-watershed-mode-map "\C-c\C-z" 'slime-switch-to-output-buffer)
(define-key superior-watershed-mode-map "\C-c\C-k" 'watershed-compile-buffer)
(define-key superior-watershed-mode-map "\C-c~"    'slime-sync-package-and-default-directory)
(define-key superior-watershed-mode-map "\C-c\M-c" 'slime-remove-notes)      ; this one doesn't seem to work in HTML-mode

; compiling templates

     ;; it would be much faster to send SWANK the entire buffer contents

(defun watershed-compile-template-buffer ()
  (interactive)
  (message "compiling")
  (watershed.compile-regions (watershed.find-sexps) nil))

(defun watershed-compile-lisp-buffer ()
  (interactive)
  (slime-compile-region (point-min) (point-max)))

(defun watershed.find-sexps ()
  (let ((depth 0)
	sexps
	start-last-sexp)
    (save-excursion
      (goto-char (point-min))
      (dotimes (n (- (point-max) (point-min)))
	(unless (watershed.at-escaped-p)
	  (case (char-after)
	    (40 (if (= depth 0)                            ; open paren
		    (setf start-last-sexp (point)))
		(incf depth))
	    (41 (decf depth)
		(if (= depth 0)	                           ; close paren
		    (push (list start-last-sexp (1+ (point))) sexps)))))
	(forward-char))
    (reverse sexps))))

(defun watershed.compile-regions (regions slime-notes)
  (message "compiling%s" (make-string (length slime-notes) 46)) ; 46 = dot character
  (when regions
    (let* ((1st% (first regions))
	   (rest (rest regions))
	   (start (first 1st%))
	   (end (second 1st%)))
      (watershed.compile-region start end rest slime-notes))))

(defun watershed.compile-region (start end more-regions slime-notes)
  (run-hook-with-args 'slime-before-compile-functions start end)
  (watershed.compile-string (format "(lambda () %s)"
				      (buffer-substring-no-properties start end))
			      (- start (length "(lambda () "))
			      (watershed-compilation-finished-continuation start end more-regions slime-notes)))

(defun watershed.compile-string (string start-offset continuation)
  (slime-eval-async
   `(swank:compile-string-for-emacs
     (watershed:prepare-watershed-template-string ,string)
     ,(buffer-name)
     ,start-offset
     ,(if (buffer-file-name) (file-name-directory (buffer-file-name))))
   continuation
   (save-excursion
     (goto-char start-offset)
     (slime-current-package))))

(defun watershed-compilation-finished-continuation (start end more-regions slime-notes)
  (lexical-let ((start start)
		(end end)
		(more-regions more-regions)
		(slime-notes slime-notes)
		(buffer (current-buffer)))		
    (lambda (result)
      (let* ((recent-notes (slime-compiler-notes))
	     (new-notes (append recent-notes slime-notes)))
	(with-current-buffer buffer
	  (if more-regions
	      (watershed.compile-regions more-regions new-notes)
	      (slime-show-note-counts new-notes nil)
	      (slime-highlight-notes new-notes)))))))