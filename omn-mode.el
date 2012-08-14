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
;;
;; Defines a major mode for editing the Manchester OWL syntax
;; Basically, this is just a bit of font locking. 



   
(defvar omn-imenu-generic-expression
  '(
    ("Class"  "Class: \\([a-zA-Z:_]+\\)" 1)
    ("ObjectProperty" "ObjectProperty: \\([a-zA-Z:_]+\\)" 1)
    ("Individual" "Individual: \\([a-zA-Z:_]+\\)" 1)
    )
    
  "Add support for imenu in omn

See `imenu-generic-expression' for details")

;; not sure if this bit is working yet!
;; (defvar omn-mode-syntax-table
;;   (let ((st (make-syntax-table)))
;;     ;; underscores are valid separators in "words"
;;     (modify-syntax-entry ?\_ "w" st)
;;     ;; for name space prefixs
;;     (modify-syntax-entry ?\: "w" st)
;;     st)
;;   "Syntax table for `omn-mode'.")

(defun omn-setup()
  (make-local-variable 'indent-line-function)
  (make-local-variable 'tab-to-tab-stop)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (setq comment-start "#")
  (setq comment-end "")

  (let ((st (syntax-table)))
    ;; underscores are valid separators in "words"
    (modify-syntax-entry ?\_ "w" st)
    ;; for name space prefixs
    (modify-syntax-entry ?\: "w" st))

  (setq imenu-generic-expression omn-imenu-generic-expression)
  (setq indent-line-function 'omn-ident-line)
  (setq tab-stop-list '(4 8 12 16 20 24)))

;; indentation engine
(defun omn-ident-line()
  (indent-line-to 
   (omn-determine-line-indent)))

(defun omn-determine-line-indent()
  (save-excursion
    (beginning-of-line)
    (save-match-data
      ;; check the first word
      (re-search-forward "\\w+" (line-end-position) t)
      (let ((word (match-string 0))
            (start (match-beginning 0)))
        (cond
         ((not word)
          (progn 
            (if (not (forward-line -1))
                (omn-determine-line-indent)
              0)))
                
         ;; basing this on font-lock isn't ideal, because only the bits of the
         ;; buffer that we have seen have been font locked. This is not a
         ;; problem for interactive use, but causes a problem when indenting
         ;; the entire buffer. 
         
         ;; if it is string, ident should be 0.
         ((eq (get-text-property start 'face)
              font-lock-string-face)
          0)
              
         ;; if it is a comment
         ((eq (get-text-property start 'face)
              font-lock-comment-face)
          ;; if there is a next line, indent the same as that
          (cond
           ((eq 0 (forward-line 1))
            (omn-determine-line-indent))
           ;; if there isn't return the same as the line before
           ((eq 0 (forward-line -1))
            (omn-determine-line-indent))
           ;; who knows?
           (t 0)))
         
         ;; if it is one of Class:, Prefix: or so on, then indent should be 0
         ((member word omn-mode-entity-keywords)
          0)
         ;; if it is Annotations:, SubClassOf: or so on, then indent should be 4
         ((member word omn-mode-property-keywords)
          4)

         ;; if it is something else, then 8
         (t 8))))))



  




(defvar omn-mode-entity-keywords
  '( 
   "Ontology:"
   "Namespace:"
   "Class:"
   "Individual:"
   "ObjectProperty:"
   "Import:"
   "Datatype:"
   "AnnotationProperty:"
   "DisjointClasses:"
   "Prefix:"
   "Alias:"
   "owl:Thing"))
  
(defvar omn-mode-property-keywords
  '(
        "EquivalentTo:"
        "SubClassOf:"
        "Annotations:"
        "Characteristics:"
        "DisjointUnion:"
        "DisjointWith:"
        "Domain:"
        "Range:"
        "InverseOf:"
        "SubPropertyOf:"
        "Types:"
        "Facts:"
        ))



;; we should move this to derived mode now, since this is far from generic
(define-generic-mode 'omn-mode
  '(("# " . nil))
  
  ;; keywords
  omn-mode-entity-keywords
  ;; a list of additional font lock info
  `(
    (
     ,(mapconcat
       (lambda(x) x)
       '("\\<some\\>"
         "\\<only\\>"
         "\\<and\\>"
         "\\<or\\>"
         "\\<exactly\\>"
         "Transitive"
         )
       "\\|")
     . font-lock-type-face)

    (
     ,(mapconcat
      (lambda(x) x)
      omn-mode-property-keywords
      "\\|")
    . font-lock-builtin-face)
    
    
    ("\\w+:\\w+" . font-lock-function-name-face)
     
    )
  
  
  
  ;; file spec
  (list "\\.omn$")
  ;; hooks
  '(omn-setup))


(add-to-list 'auto-mode-alist
             '("\\.pomn$" . omn-mode))



(provide 'omn-mode)

;; interaction with a reasoner.....
;; Define a struct using CL, which defines a command. Then send this to the command line 
;; program as a single key-value pair line. 
;; 
;; Write a parser for this in Java.
;; Write a "command" interface, use annotation to mark each of the command setMethods. 
;; 
;; Have the command interface return results between tags as lisp. We can eval
;; this, and get the result in that way. 