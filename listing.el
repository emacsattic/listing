;;; listing.el --- show columnized view of list elements

;; Copyright (C) 2010  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20100605
;; Updated: 20100803
;; Version: 0.1.2+
;; Homepage: https://github.com/tarsius/listing
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package implements the generic `listing-mode' which another mode
;; can derive from to display a columnized list of the elements of a list.
;; The elements should be some kind of composed values.

;;; Code:

(require 'header-button)
(require 'map-progress)

(defvar listing-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map [?v] 'listing-view-element)
    (define-key map [?m] 'listing-mark-element)
    map))

(define-derived-mode listing-mode special-mode nil
  "Parent major mode from which other listing modes derive."
  (kill-all-local-variables)
  (use-local-map listing-mode-map)
  (hl-line-mode 1)
  (setq truncate-lines t
	buffer-read-only t))

(defun listing-create (value buffer mode columns column predicates)
  "Insert elements of the list VALUE into BUFFER, one per line.

MODE is the major mode used in BUFFER.  If it is nil `listing-mode' is
used.

COLUMN is the column by which the elements are sorted initialy.  COLUMNS
specifies what parts are inserted and can optionally be used to format
the output of each column individually. It has the form:

  ((HEADER LENGTH ACCESSOR)...)

HEADER is a string used as label in the header line for the respective
column.  ACCESSOR is a function used to extract the value for the
respective column from each of the elements of VALUE.  It may also be a
keyword in which case function `plist-get' is used to extract the value
to be inserted.  LENGTH defined the minimal length of the column."
  (with-current-buffer (or buffer (current-buffer))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (listing-insert columns (listing-match predicates value))
      (listing-sort columns column)
      (funcall (or mode 'listing-mode))
      (setq header-line-format (listing-format-header columns))
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(define-button-type 'listing-header
  :supertype 'header
  :action (lambda (button)
	    (listing-sort (header-button-get button :columns)
			  (header-button-label button))))

(defun listing-format-header (columns)
  (let ((len 2))
    (concat "   "
	    (mapconcat
	     (lambda (col)
	       (concat
		(format-header-button (car col)
		 :type 'listing-header
		 :columns columns
		 'help-echo (concat "mouse-1: Sort by "
				    (downcase (car col))))
		(propertize " "
		 'display `(space :align-to ,(1+ (incf len (cadr col))))
		 'face 'fixed-pitch)))
	     columns " "))))

(defun listing-format-element (columns value)
  (let ((len 2))
    (concat "  "
	    (mapconcat
	     (lambda (col)
	       (let ((val (funcall (caddr col) value)))
		 (concat
		  (cond ((null val) "-?-")
			((stringp val) val)
			(t (prin1-to-string val)))
		  (propertize "\037"
		   'display `(space :align-to ,(1+ (incf len (cadr col))))
		   ))))
	     columns " ")
	    "\n")))

(defun listing-match (predicates value)
  value) ; TODO

(defun listing-insert (columns value)
  (mapc-with-progress-reporter
   "Inserting elements..."
   (lambda (elt)
     (insert (propertize (listing-format-element columns elt)
			 :listing-element elt
			 'point-entered 'listing-line-entered)))
   value))

(defun listing-sort (columns column &optional from to)
  (interactive
   (when last-input-event
     (mouse-select-window last-input-event)
     (let ((object (posn-object (event-start last-input-event))))
       (list column (get-text-property (cdr object) 'column-name
				       (car object))))))
  (let ((inhibit-read-only t)
	(inhibit-point-motion-hooks t)
	(regexp "^. "))
    (while columns
      (if (equal (caar columns) column)
	  (setq regexp (concat regexp "\\([^\037]+\\)\037.*$")
		columns nil)
	(setq regexp (concat regexp "[^\037]+\037")
	      columns (cdr columns))))
    (sort-regexp-fields nil regexp "\\1"
			(or from (point-min))
			(or to (point-max)))))

(defun listing-line-entered (old new)
  (let ((old-elt (get-text-property old :listing-element))
	(new-elt (get-text-property new :listing-element))
	window buffer)
    (unless (eq old-elt new-elt)
      (walk-windows (lambda (win)
		      (with-current-buffer (window-buffer win)
			(when (and listing-buffer-element (not window))
			  (setq window win)))))
      (when window
	(setq buffer (window-buffer window))
	(listing-view-element)
	;; Motion hook functions get called twice by design.  In case this
	;; is the second time this function is called it would be wrong to
	;; kill the element buffer.
	(unless (equal new-elt (with-current-buffer buffer
				 listing-buffer-element))
	  (kill-buffer buffer))))))

(defun listing-view-element ()
  (interactive)
  (funcall listing-view-element-function
	   (get-text-property (point) :listing-element)))

(defvar listing-view-element-function nil)
(make-variable-buffer-local 'listing-view-element-function)

(defvar listing-buffer-element nil)
(make-variable-buffer-local 'listing-buffer-element)

(defvar listing-buffer-element-type nil)
(make-variable-buffer-local 'listing-buffer-element-type)

(provide 'listing)
;;; listing.el ends here
