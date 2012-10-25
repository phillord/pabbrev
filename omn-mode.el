;; omn-mode.el --- Support for OWL Manchester Syntax

;; Version: 1.0

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Maintainer: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Website: http://www.russet.org.uk/blog

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


;; indentation engine
(defun omn-indent-line()
  (indent-line-to 
   (omn-determine-line-indent)))

(defun omn-determine-line-indent()
  (save-excursion
    (beginning-of-line)
    (save-match-data
      ;; check the first word
      
      (let* ((match (re-search-forward "\\w+" (line-end-position) t))
             (word (if match 
                       (match-string 0)
                     "")))
                     
        (cond
         ;; ((not match)
         ;;  (progn 
         ;;    (if (not (forward-line -1))
         ;;        (omn-determine-line-indent)
         ;;      0)))
                
         ;; if it is string, ident should be 0.
         ((nth 3 (syntax-ppss (point)))
          0)
   
         ;; if it is a comment
         ((nth 4 (syntax-ppss (point)))
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

(add-to-list 'auto-mode-alist
             '("\\.pomn$" . omn-mode))

(add-to-list 'auto-mode-alist
             '("\\.omn$" . omn-mode))

(defvar omn-font-lock-defaults
  `(,
    (concat "\\_<" 
            (regexp-opt omn-mode-entity-keywords t)
            "\\_>")
    (,(mapconcat
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
    ("\\w+:\\w+" . font-lock-function-name-face)))
    

(defvar omn-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; string quotes
    (modify-syntax-entry ?\" "\"" st)
    ;; This is a bit underhand, but we define the < and > characters to be
    ;; "generic-string" delimiters. This results in fontification for URLs
    ;; which is no bad thing. Additionally, it makes the comment character
    ;; work, as "#" is a valid in a URL. The semantics of this isn't quite
    ;; right, because the two characters are not paired. So <url> is
    ;; recognised, but so is <url< or >url>
    (modify-syntax-entry ?\< "|" st)
    (modify-syntax-entry ?\> "|" st)
    ;; define comment characters for syntax
    (modify-syntax-entry ?\# "<" st)
    (modify-syntax-entry ?\n ">" st)
    ;; underscores are valid separators in "words"
    (modify-syntax-entry ?\_ "w" st)
    ;; for name space prefixs
    (modify-syntax-entry ?\: "w" st)
    st))

(defun omn-mode-electric-indent()
  (interactive)
  (self-insert-command 1)
  (omn-mode-indent-here))

(defun omn-mode-indent-here()
  (let ((m (point-marker)))
    (omn-indent-line)
    (goto-char (marker-position m))))

(defun omn-mode-electric-newline()
  (interactive)
  (newline)
  (save-excursion
    (forward-line -1)
    (omn-indent-line)))

(define-derived-mode omn-mode fundamental-mode "Omn"
  "Doc string to add"

  ;; font-lock stuff
  (setq font-lock-defaults
        '(omn-font-lock-defaults))

  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  ;; set up commenting
  (setq comment-start "#")
  (setq comment-end "")
  ;; no idea what this is about -- stolen from generic
  (setq comment-start-skip "#+\\s-*")

  (set-syntax-table omn-mode-syntax-table)
  
  (setq imenu-generic-expression omn-imenu-generic-expression)

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'omn-indent-line))


;; need to bind to return as well
(mapc
 (lambda(x)
   (define-key omn-mode-map x 'omn-mode-electric-indent))
 `(" " "," ":"))

(define-key omn-mode-map (kbd "RET") 'omn-mode-electric-newline)




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

