;;; leiningen.el --- Run leiningen in a compile buffer with completion

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Maintainer: Phillip Lord <phillip.lord@newcastle.ac.uk>
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

;; Commentary:
;;
;; Runs leiningen -- a build tool for Clojure -- within a compile buffer.
;; Useful for testing, gathering dependencies and so on.

;;; Code:
(defvar leiningen-command "lein")

(defvar leiningen-tasks
  '("clean" "compile" "deploy" "deps" "do" "help" "install"
    "jack-in" "jar" "javac" "marg" "new" "plugin" "pom" "repl"
    "retest" "run" "search" "show-profiles" "swank" "test"
    "trampoline" "uberjar" "upgrade" "version" "with-profile"))

(defun leiningen ()
  (interactive)
  (compile (concat 
            leiningen-command " "   
            (completing-read "Task: " leiningen-tasks))))


(provide 'leiningen)


;;; leiningen.el ends here


