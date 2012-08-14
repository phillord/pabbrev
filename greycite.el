;; greycite.el --- integrate referencing with the Greycite service

;; Version: 0.1

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
;; This code in it's early stages. The greycite service aims to make the web
;; citable by searching and storing basic metadata about articles on the web.
;; This package helps to integrate Emacs' own referencing capabilities (reftex,
;; bibtex and the like) with greycite.

;;
;; There are two main pieces of functionality. First you can use greycite.el
;; to resolve DOIs or URLs into an equivalent piece of bibtex. This is useful
;; because it provides the metadata for inserting references, which you can do
;; using the reftex package which this file modifies to insert citations in
;; the correct format. 
;;
;; The main entry points for the bibtex functionality are
;; `greycite-bibtex-url' which transforms a URL into a bibtex record, or
;; `greycite-bibtex-update' which updates it. `greycite-bibtex-update' is
;; currently somewhat destructive of updates that have been made manually, so
;; you should be careful if this is the case. `greycite-bibtex-doi' and
;; `greycite-bibtex-doi-update' do similar jobs for DOIs.
;;
;; Reftex support is added automatically to adoc-mode. If you prefer to use
;; someother mode than adoc, `greycite-asciidoc-reftex-support' will turn this
;; on. 
;;

;;; Code:

;;  
;;
;; reftex support for asciidoc mode
;;

(add-hook 'adoc-mode-hook
          'greycite-asciidoc-reftex-support)

(defvar greycite-reftex-citation-override nil)
(defvar greycite-adoc-kblog-cite-format 
  '(
    (?\C-m . "kurl:")
    (?h . "http:")
    (?j . "http:[]"))
  )

(defvar greycite-default-bibliographies
  '("~/documents/bibtex/phil_lord_refs.bib" 
    "~/documents/bibtex/phil_lord/journal_papers.bib"
    "~/documents/bibtex/phil_lord/conference_papers.bib"
    "~/documents/bibtex/urls.bib"
    "~/documents/bibtex/russet.bib"
    ))


(defun greycite-asciidoc-reftex-support()
  (interactive)
  (reftex-mode 1)
  (make-local-variable 'greycite-reftex-citation-override)
  (setq greycite-reftex-citation-override t)
  (make-local-variable 'reftex-default-bibliography)
  (make-local-variable 'reftex-cite-format)
  (setq reftex-cite-format
        greycite-adoc-kblog-cite-format)
  (setq reftex-default-bibliography greycite-default-bibliographies))

(defadvice reftex-format-citation (around greycite-asciidoc-around activate)
  "Alter citation style for kcite"
  (if greycite-reftex-citation-override
      (progn 
        (setq ad-return-value (greycite-reftex-format-citation entry format)))
    ad-do-it))

;; we can't just use reftex-format-citation -- it has will template with most
;; keys, but not DOI or URL. So just override it. 
(defun greycite-reftex-format-citation( entry format ) 
  (cond 
   ;; the template strings are duplicated in phil-kblog-cite-format
   ((string= format "kurl:")
    (or 
     (greycite-reftex-or-false 
      entry "doi" "kurl:dx.doi.org/")
     (greycite-reftex-or-false 
      entry "url" "kurl:" 
      (lambda(url)
	(substring url 7))
      )))
   ((string= format "http:")
    (reftex-get-bib-field "url" entry))
   ((string= format "http:[]")
    (concat (reftex-get-bib-field "url" entry) "[]"))
   ))


(defun greycite-reftex-or-false(entry field prefix &optional transform)
  (let ((field-val 
         (reftex-get-bib-field field entry)))
    (if (not (string= field-val ""))
        (format " %s%s[]" prefix 
                (if transform 
                    (funcall transform field-val)
                  field-val))
      nil)))

(defadvice reftex-format-bib-entry (around greycite-asciidoc-format-bib activate)
  (setq ad-return-value (greycite-reftex-entry-display entry ad-do-it)))

(defun greycite-reftex-entry-display(entry formatted)
  (let*
      ((url (reftex-get-bib-field "url" entry))
       (doi (reftex-get-bib-field "doi" entry))
       (id 
        ;; DOI if we have it, or URL
        (if (not (string= doi ""))
            doi
          url)))
    (put-text-property 0 (length id) 'face reftex-bib-extra-face id)
    ;; chop of last new line
    (concat (substring formatted 0 -1)
            "     "  id "\n\n")))



;; bibtex stuff
(defun greycite-bibtex-from-greycite(url)
  (save-excursion 
    (set-buffer 
     (url-retrieve-synchronously 
      (concat 
       "http://greycite.knowledgeblog.org/bib?uri="
       url)))
    (goto-char (point-min))
    (delete-region 
     (point-min)
     (search-forward "\n\n"))
    ;; if there isn't a title, then use the URL or nothing appears in reftex.
    (let ((entry 
           (bibtex-parse-entry)))
      (when (not 
             (assoc "title" entry))
        (search-forward ",")
        (insert "title =")
        (insert (cdr 
                 (assoc "url" entry)))
        (insert ",")))
    (buffer-string)))
 

(defun greycite-bibtex-url()
  (interactive)
  (let* ((url (thing-at-point 'url))
         (bounds (bounds-of-thing-at-point 'url))
         (bibtex (greycite-bibtex-from-greycite
                  url)))
    (delete-region (car bounds) (cdr bounds))
    (insert bibtex)
    (bibtex-clean-entry)
    (bibtex-fill-entry)))

(defun greycite-bibtex-update()
  (interactive)
  (save-restriction
    (bibtex-narrow-to-entry)
    (goto-char (point-min))
    (let* ((entry (bibtex-parse-entry))
           (url 
            (substring
             (cdr (assoc "url" entry)) 1 -1))
           (key (cdr (assoc "=key=" entry)))
           (update (greycite-bibtex-from-greycite url)))
      (delete-region (point-min) (point-max))
      (insert update)
      ;; fix the key in case it has changed
      (goto-char (point-min))
      (search-forward "{")
      (zap-to-char 1 ?,)
      (insert (concat key ","))

      (bibtex-clean-entry)
      (bibtex-fill-entry))))


;; I can't get url-retrieve-synchronously to do content negotiation, so give
;; up and doi it in PHP instead
(defun greycite-bibtex-from-doi(doi)
  (save-excursion
    (set-buffer 
     (url-retrieve-synchronously 
      (concat 
       "http://greycite.knowledgeblog.org/resolve/"
       doi)))
    (goto-char (point-min))
    (delete-region
     (point-min)
     (search-forward "\n\n"))
    (buffer-string)))


(defun greycite-bibtex-doi()
  (interactive)
  ;; thing at point URL is about right, but stuffs "http:" on the beginning. 
  ;; hence substring
  (let* ((doi (thing-at-point 'line))
         (bounds (bounds-of-thing-at-point 'line))
         (bibtex (greycite-bibtex-from-doi doi)))
    (delete-region (car bounds) 
                   (cdr bounds))
    (insert bibtex)
    (bibtex-clean-entry)
    (bibtex-fill-entry)))

;; (nearly) identical to phil-bibtex-update
(defun greycite-bibtex-doi-update()
  (interactive)
  (save-restriction
    (bibtex-narrow-to-entry)
    (goto-char (point-min))
    (let* ((entry (bibtex-parse-entry))
           (doi 
            (substring
             (cdr (assoc "DOI" entry)) 1 -1))
           (key (cdr (assoc "=key=" entry)))
         (update (greycite-bibtex-from-doi doi)))
      (delete-region (point-min) (point-max))
      (insert update)
      ;; fix the key in case it has changed
      (goto-char (point-min))
      (search-forward "{")
      (zap-to-char 1 ?,)
      (insert (concat key ","))

      (bibtex-clean-entry)
      (bibtex-fill-entry))))
  

(defun greycite-bibtex-region-from-greycite(start end)
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-max))
    (while (> (point) (point-min))
        (progn
          (save-excursion
            (greycite-bibtex-url))
          (forward-line -1)
          (sit-for 0.1)))))
        
(defun greycite-bibtex-buffer-from-greycite()
  (interactive)
  (greycite-bibtex-region-from-greycite 
   (point-min) (point-max)))

(provide 'greycite)