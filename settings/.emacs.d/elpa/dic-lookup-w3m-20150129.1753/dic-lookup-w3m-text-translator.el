;;; dic-lookup-w3m-text-translator.el --- look up dictionaries on the Internet

;; Copyright (C) 2008, 2009, 2012  mcprvmec

;; Author: mcprvmec

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Convert text-translator's site data into w3m-search's format.

;;; Code:

(require 'text-translator)

(defvar dic-lookup-w3m-search-engine-alist '())
(mapc
 #'(lambda (elem)
    (add-to-list
     'dic-lookup-w3m-search-engine-alist
     (list
      (nth 0 elem)
	(format "http://%s%s" (nth 1 elem) (car (split-string (nth 2 elem))))
	(nth 4 elem)
	(nth 3 elem))))
 text-translator-site-data-alist)

(defun dic-lookup-w3m-filter-text-translator (url func-or-regexp)
  (let (str)
    (if (functionp func-or-regexp)
	(setq str (funcall func-or-regexp))
      (goto-char (point-max))
      (re-search-backward func-or-regexp nil t)
      (setq str (match-string 1)))
    (if (null str)
	(message "filtering failed. %s" func-or-regexp)
      (goto-char (point-min))
      (w3m-filter-delete-regions "" "<body" "</body>")
      (insert (format "<body><p>%s</p><body>" str)))))

(defadvice text-translator-extract-tag-exclusion-string
  (before point-max (regex &optional dont-convert-br))
	   (goto-char (point-max)))
(ad-activate 'text-translator-extract-tag-exclusion-string)

(eval-after-load "w3m-filter"
  '(let (list url)
     (dolist (elem text-translator-site-data-alist)
       (unless (assoc
		(format "\\`http://%s%s"
			(regexp-quote (nth 1 elem))
			(regexp-quote (car (split-string (nth 2 elem)))))
		w3m-filter-rules)
	 (add-to-list 'list elem)))
     (dolist (elem list)
       (setq url (format "\\`http://%s%s"
			 (regexp-quote (nth 1 elem))
			 (regexp-quote (car (split-string (nth 2 elem))))))
       (add-to-list 'w3m-filter-rules
		    (list url
			  'dic-lookup-w3m-filter-text-translator
			  (nth 5 elem))
		    t)
       (if (string-match "en$" (car elem))
	   (progn
	     (add-to-list 'w3m-filter-rules
			  (list url
				'dic-lookup-w3m-filter-eword-anchor
				"alc")
			  t))))))

(provide 'dic-lookup-w3m-text-translator)

;;; dic-lookup-w3m-text-translator.el ends here
