;;; centering.el -- Keep the cursor in the centre at all times. 

;; $Revision: 1.14 $
;; $Date: 2004/12/11 16:48:14 $

;; This file is not part of Emacs

;; Author: Phillip Lord <p.lord@russet.org.uk>
;; Maintainer: Phillip Lord <p.lord@russet.org.uk>
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

;; This mode ensures that the cursor is always in the center of the
;; display. This can be useful at times if, for example, you are
;; trying to get lots of stuff onto one screen for a screenshot, or to
;; read without using the keyboard. 
;;
;; It has one major entry point which is `centering-mode' which turns
;; on a minor mode. 
;;
;; It currently works by recentering the display after the line
;; position has changed. There is a delay before this happens or the
;; constant redisplaying makes the emacs look horrible. Set
;; `centering-timer-delay' to change the length of the delay. Setting
;; it to 0 is permissible. 



;;; Todo
;;
;; Well it doesn't work. So fix the centering-recenter function. Also 
;; this should only use a single timer. When switching on, check for
;; timer, start if not. When switching off check whether there are any
;; centering buffers left open, if not kill it. 
;;
;; The current logic is imperfect, because if the a key is
;; autorepeated, then the system will not update when the up key is
;; removed. Perhaps I should move back to the old delay system. 


(define-minor-mode centering-mode
  "Keep the cursor in the center at all times"
  nil
  " Cr"
  nil)

(add-hook 'centering-mode-on-hook 
          'centering-mode-on)
(add-hook 'centering-mode-off-hook
          'centering-mode-off)

(defun centering-mode-on()
  (add-hook 'post-command-hook 'centering-post-command-hook nil t))

(defun centering-mode-off()
  (remove-hook 'post-command-hook 'centering-post-command-hook t))

(defun centering-post-command-hook()
  (when centering-timer
      (cancel-timer centering-timer))
  (run-with-timer centering-delay nil
                  'centering-recenter))

(defun centering-recenter()
  (unless (= centering-position
             (line-beginning-position))
    (setq centering-position (line-beginning-position))
    (recenter)))


(defvar centering-delay 0.1)
(defvar centering-timer nil)
(defvar centering-position 1)
(make-variable-buffer-local 'centering-position)