;;; eval-pulse.el --- Pulse lisp forms as they are evaled

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Maintainer: Phillip Lord <phillip.lord@newcastle.ac.uk>

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Makes Emacs "pulse" your lisp forms when you eval them. This gives you a
;; clear visual indication that you have evaled something and, when evaling
;; subforms an indication of what you have evaled.

;; To enable this, add
;;
;; (require 'eval-pulse)
;;
;; to your .emacs, and enable it via a hook in all appropriate buffers.

;; To work this package advices `eval-buffer' and `eval-region', as these
;; functions are called directly from the menu system. Surely, it can't be a
;; good idea to change such low-level functions, a core part of the lisp
;; loading system of Emacs for such a piece of cheap eye-candy?

(require 'pulse)

;;; Code:
(defvar eval-pulse-pulses 0)
(defvar eval-pulse-form nil)

;; evaling `eval-pulse-depth' causes all sorts of problems, so reset here with
;; eval-pulse-depth 1
;;(setq eval-pulse-depth 1)
(defvar eval-pulse-depth 0)
(defvar eval-pulse-delay 0.01)
(defvar eval-pulse-iterations 4)

;; probe to see whether we can pulse the foreground or not. The current
;; version of pulse sets `pulse-highlight-face' to have a background, while my
;; hacked version gives this face no characteristics.
(defvar eval-pulse-can-pulse-foreground
  (not (face-background 'pulse-highlight-face)))

(defface eval-pulse-highlight-start-face
  '((((class color) (background dark))
     (:foreground "#FF0000"))
    (((class color) (background light))
     (:foreground  "#FF0000")))
  "Face used at beginning of a highlight."
  :group 'eval-pulse)

;; pulse is hard-coded for background -- bummer
(defun eval-pulse-momentary-highlight-region
  (start stop)
  "Pulse the region."
  ;; (message "Pulsing: %s:%s:%s:%s" (incf eval-pulse-pulses)
  ;;           eval-pulse-form
  ;;           eval-pulse-depth
  ;;           start stop)
  (let ((pulse-delay eval-pulse-delay)
        (pulse-iterations eval-pulse-iterations))
    ;; only actually pulse when we are at an pulse depth of 1, otherwise we
    ;; get multiple flashes
    (when (and eval-pulse-mode
               (= 1 eval-pulse-depth))
      (pulse-momentary-highlight-region
       start stop
       (if eval-pulse-can-pulse-foreground
           'eval-pulse-highlight-start-face
         'pulse-highlight-start-face)))))

(defmacro eval-pulse-one-pulse (form &rest body)
  "Only allow one pulse at a time. The various adviced eval forms
tend to call each other so this is necessary to avoid flashing screens."
  `(unwind-protect
       (progn (setq eval-pulse-depth
                    (+ 1 eval-pulse-depth))
              (setq eval-pulse-form ,form)
              ,@body)
     (setq eval-pulse-depth
           (- eval-pulse-depth 1))))

(defun eval-pulse-last-sexp (position)
  "Pulse the last sexp."
  (eval-pulse-momentary-highlight-region
   (save-excursion
     (goto-char position)
     (backward-sexp)
     (point))
   (save-excursion
     (goto-char position)
     (backward-sexp)
     (forward-sexp)
     (point))))

(defun eval-pulse-defun (position)
  "Pulse the defun at point."
  (eval-pulse-momentary-highlight-region
   (save-excursion
     (goto-char position)
     ;; we move to the end to mimic the behaviour of eval-defun -- we eval
     ;; the defun surrounding point *or* the next one
     (end-of-defun)
     (beginning-of-defun)
     (point))
   (save-excursion
     (goto-char position)
     (end-of-defun)
     (point))))

(defun eval-pulse-region (start stop)
  "Pulse the current region"
  (eval-pulse-momentary-highlight-region
   start stop))

(defun eval-pulse-buffer (buffer)
  "Pulse the current buffer"
  (with-current-buffer
      (or buffer
          (current-buffer))
    (eval-pulse-momentary-highlight-region
     (point-min) (point-max))))

;; advice forms. These do not force loading of the functions that the advice.
;; which can be adviced afterwards.

;; Elisp
(defadvice eval-buffer
  (around pulse-eval-buffer activate)
  "Add a pulsing effect to the region evaled."
  (eval-pulse-one-pulse
   'eval-buffer
   ad-do-it
   (eval-pulse-buffer
    (ad-get-arg 0))))

(defadvice eval-region
  (around pulse-eval-region activate)
  "Add a pulsing effect to the region evaled."
  (eval-pulse-one-pulse
   'eval-region ad-do-it
   (eval-pulse-region
    (ad-get-arg 0)
    (ad-get-arg 1))))

(defadvice eval-defun
  (around pulse-eval-eval-defun activate)
  "Add a pulsing effect to the region evaled."
  (eval-pulse-one-pulse
   'eval-defun
   (let ((point (point)))
     ad-do-it
     ;;(message "Evaled defn about to pulse")
     (eval-pulse-defun point))))

(defadvice eval-last-sexp
  (around pulse-eval-last-sexp activate)
  "Add a pulsing effect to the region evaled."
  (eval-pulse-one-pulse
   'eval-last-sexp
   (let ((point (point)))
     ad-do-it
     (eval-pulse-last-sexp point))))

;; inferior lisp
(defadvice lisp-eval-defun
  (around pulse-lisp-eval-defun activate)
  "Add a pulsing effect to the region evaled."
  (eval-pulse-one-pulse
   'lisp-eval-defun
   (let ((point (point)))
     ad-do-it
     (eval-pulse-eval-defun point))))

(defadvice lisp-eval-last-sexp
  (around pulse-lisp-eval-last-sexp activate)
  "Add a pulsing effect to the region evaled."
  (eval-pulse-one-pulse
   'lisp-eval-last-sexp
   (let ((point (point)))
     ad-do-it
     (eval-pulse-last-sexp point))))

;; clojure
(defadvice cider-eval-expression-at-point
  (around pulse-cider-eval-expression-at-point activate)
  "Add a pulsing effect to the region evaled."
  (eval-pulse-one-pulse
   'cider-eval-expression-at-point
   (let ((point (point)))
     ad-do-it
     (eval-pulse-defun point))))

(defadvice cider-eval-ns-form
  (around pulse-cider-eval-ns-form activate)
  "Add a pulsing effect to the region evaled."
  ;; clojure-find-ns is used by cider-eval-ns-form so it will be available
  ;; before this runs
  (eval-pulse-one-pulse
   'cider-eval-ns-form
   ad-do-it
   (when (clojure-find-ns)
     ;; truely evil use of match data from clojure-find-ns
     (save-excursion
       (eval-pulse-defun (match-beginning 0))))))

(defadvice cider-load-current-buffer
  (around pulse-cider-load-current-buffer activate)
  "Add a pulsing effect to the region evaled."
  (eval-pulse-one-pulse
   'cider-load-current-buffer
   ad-do-it
   (eval-pulse-buffer (current-buffer))))

(defadvice cider-eval-last-expression
  (around pulse-cider-eval-last-expression activate)
  "Add a pulsing effect to the region evaled."
  (eval-pulse-one-pulse
   'lisp-eval-last-sexp
   (let ((point (point)))
     ad-do-it
     (eval-pulse-last-sexp point))))


;; End Advice

(defvar eval-pulse-disabled nil)

(defun eval-pulse-disable ()
  "Deactivate all advice enabling eval-pulse.
This is meant for emergency situations."
  (interactive)
  (setq eval-pulse-disabled t)
  (ad-deactivate-regexp "pulse-.*"))

(defun eval-pulse-enable ()
  "Activate all advice enabling eval-pulse.
This is the default and only needs to be called explicitly after
`eval-pulse-disable'"
  (interactive)
  (setq eval-pulse-disabled nil)
  (ad-activate-regexp "pulse-.*" t))

(define-minor-mode eval-pulse-mode
  "Pulses lisp expressions when they are evaluated"
  t " ep" nil
  (when
      (and eval-pulse-disabled
           (not eval-pulse-mode))
    (message "Eval pulse mode is enabled, but has been deactivated")))

(provide 'eval-pulse)
;;; eval-pulse.el ends here
