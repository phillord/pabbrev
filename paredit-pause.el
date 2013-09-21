;;; paredit-pause.el -- Easily pause and unpause paredit

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Maintainer: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Keywords: paredit
;; Version: 1.0


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
;; You know how it is. You're wandering happily through your syntax tree, all
;; your sexps are balanced, your code is looking lovely. Then, suddenly it
;; happens, your fixing one of your functions, and bang, your barf one sexp
;; too many, with C-left. Oh no! So, you C-right before realising that it's
;; not the opposite. Half your file gets slurped, and probably several other
;; files that you haven't even opened. You try splitting, wrapping, raising,
;; it's just gets worse and worse. Randomly symbols get spewed across your
;; source code, empty forms, and strings everywhere; you look at the hideous
;; ruin of your work; look at you works, ye mighty, and despair.

;; The solution, is M-x paredit-mode, fix stuff, and them M-x paredit-mode.
;; But this takes too long, and is painful, hence paredit-pause was born.

;; Paredit allows you to toggle paredit-mode on and off with a single
;; keypress. When paredit-mode is off, it is replaced with paredit-pause-mode,
;; which you allows you to toggle paredit-mode back on again.

;;; Installation:
;;
;; Add (require 'paredit-pause) to your .emacs. Of if you autoload paredit,
;; then
;;
;; (eval-after-load "paredit.el"
;;    '(require 'paredit-menu))
;;
;; will achieve the same effect.

(require 'paredit)

;;; Code:

(defun paredit-pause-unpause ()
  "Disable command `paredit-mode' and enable command `paredit-pause-mode'."
  (interactive)
  (paredit-mode 1)
  (paredit-pause-mode 0))

(defun paredit-pause-pause ()
  "Disable mode command `paredit-pause-mode' and enable command `paredit-mode'."
  (interactive)
  (paredit-mode 0)
  (paredit-pause-mode 1))

(define-key paredit-mode-map "\C-c!" 'paredit-pause-pause)

(define-minor-mode paredit-pause-mode
  "A minor mode which allows easy re-enabling of paredit."
  nil " paredit-pause"
  '(("\C-c!" . paredit-pause-unpause)))

(provide 'paredit-pause)
;;; paredit-pause.el ends here
