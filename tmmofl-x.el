;; tmmofl-x.el - major mode library for tmmofl
;; $Revision: 1.3 $
;; $Date: 2000/06/19 22:03:27 $

;; This file is not part of Emacs

;; Author: Phillip Lord<p.lord@hgmp.mrc.ac.uk>
;; Maintainer: Phillip Lord
;; Keywords: minor mode, font lock, toggling, tmmofl, 

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


;;;
;; Commentary
;;
;; All documentation for this file is available in the tmmofl.el file



(provide 'tmmofl-x)

(defvar tmmofl-jde-mode-actions
  '((font-lock-comment-face 
     (lambda()
       (progn
         (abbrev-mode 0)
         (auto-fill-mode 1)))
     (lambda()
       (progn
         (abbrev-mode 1)
         (auto-fill-mode 0))))
    
    (font-lock-string-face
     (lambda()
       (abbrev-mode 0))
     (lambda()
       (abbrev-mode 1)))))
    

(defvar tmmofl-clojure-mode-actions
  '((font-lock-comment-face 
     (lambda()
       (progn
         (auto-fill-mode 1)))
     (lambda()
       (progn
         (auto-fill-mode 0))))
    
    (font-lock-string-face
     (lambda()
       (auto-fill-mode 1))
     (lambda()
       (auto-fill-mode 0)))))
    
  




