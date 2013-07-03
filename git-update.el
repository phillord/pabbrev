;;; git-update.el --- Clone and pull a git repo with Emacs source

;; Version: 1.0

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Maintainer: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Website: http://www.russet.org.uk

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
;; This package ensures that a git repository is cloned, and updates is
;; regularly. This is useful for using Emacs packages where you want to
;; stay on the bleeding edge. Think of it as a poor-mans MELPA or el-get.
;;
;; It has the advantage over el-get that it's pretty fast -- it's 100 lines
;; long rather than 1000's. Obviously, it's not as clever either. And it has
;; the advantage over package.el that it can be used unattended. I use it in
;; conjunction with `use-package' to maintain many of my external packages; I
;; maintain a single .emacs files which I synchronize between machines, with
;; the actual installation taking place automatically, as in this example:
;;
;; (use-package clojure-mode
;;   :init (git-update "git://github.com/technomancy/clojure-mode.git"
;;            "~/emacs/packages/development")
;;   :mode ("\\.clj$" . clojure-mode)
;;   :defer t
;;   :config
;;   (add-hook 'clojure-mode-hook 'clojure-test-maybe-enable)
;;
;; It has a single entry point `git-update' which takes a repo location and,
;; optionally a location in which to install. Adding to the load path takes
;; place automatically.

;;; Code:
(defvar git-update-install-root (concat user-emacs-directory "git-packages"))
(defvar git-update-frequency 1)
(defvar git-update-buffer (get-buffer-create "*git-update*"))

(defun git-update (repo &optional location)
  "Check if the REPO has been cloned into `git-update-install-root'.

If not then run git clone. If not check whether the file
.git/FETCH_HEAD is older than `git-update-frequency' days old. If
it is run git pull. Otherwise, do nothing
Optional argument LOCATION is the directory into which the REPO will be cloned, 
otherwise `git-update-install-root' will be used."
  (let* ((location (or location git-update-install-root))
         (project-dir
          (concat (or location
                      git-update-install-root) "/"
                      (git-update-dir-from-git repo))))
    (if (file-exists-p project-dir)
        (git-update-maybe project-dir)
      (git-update-clone repo location)
      (git-update-maybe project-dir))
    (let ((default-directory project-dir))
      (normal-top-level-add-subdirs-to-load-path))
    (add-to-list 'load-path project-dir)
    project-dir))

(defun git-update-dir-from-git (git-location)
  (first
   (last
    (split-string
     (substring git-location 0 -4) "/" ))))

(defun git-update-clone (repo location)
  (display-buffer git-update-buffer)
  (set-buffer git-update-buffer)
  (goto-char (point-max))
  (message (format "Clone: %s..." repo))
  (insert (format "Clone: %s\n" repo))
  (cd location)
  (call-process "git" nil git-update-buffer t "clone"
                repo)
  (message (format "Clone: %s...done" repo)))

(defun git-update-maybe (git-location)
  ;; after a clone, FETCH_HEAD doesn't exist. So, we can't tell when the clone
  ;; happened. So, do a pull anyway.
  (let ((fetch-head (concat git-location "/.git/FETCH_HEAD")))
    (if (not (file-exists-p fetch-head))
        (git-update-do git-location)
      (let ((age
             (-
              (nth 1
                   (current-time))
              (nth 1
                   (nth 5
                        (file-attributes
                         (concat git-location "/.git/FETCH_HEAD" )))))))
        (if (> age (* git-update-frequency 60 60 24))
            (git-update-do git-location)
          (with-current-buffer
            git-update-buffer
            (goto-char (point-max))
            (insert (format "Not updating %s\n" git-location))))))))

(defun git-update-do (git-location)
  (display-buffer git-update-buffer)
  (set-buffer git-update-buffer)
  (goto-char (point-max))
  (insert (format "Updating: %s\n" git-location))
  (goto-char (point-max))
  (cd git-location)
  (call-process "git" nil git-update-buffer t "pull")
  (message (format "Updating: %s...done" git-location)))

(provide 'git-update)
;;; git-update.el ends here
