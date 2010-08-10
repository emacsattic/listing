;;; listing.el --- show columnized view of list elements

;; Copyright (C) 2010  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20100605
;; Updated: 20100810
;; Version: 0.1.3+
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
    (define-key map [?w] 'listing-widen)
    map)
  "Keymap for Listing mode.")

(define-derived-mode listing-mode special-mode nil
  "Parent major mode from which other listing modes derive."
  (kill-all-local-variables)
  (use-local-map listing-mode-map)
  (hl-line-mode 1)
  (setq truncate-lines t
	buffer-read-only t
	x-stretch-cursor nil
	buffer-invisibility-spec nil))

(defun listing-create (value buffer mode columns
			     &optional column format predicates)
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
    (let ((inhibit-read-only t)
	  (inhibit-point-motion-hooks t))
      (erase-buffer)
      (listing-insert columns
		      (listing-match (or predicates '(identity)) value))
      (funcall (or mode 'listing-mode))
      (listing-sort columns column)
      (setq header-line-format (listing-format-header columns)
	    listing-buffer-columns columns)
      (when format
	(setq listing-format-element-function format))
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(define-button-type 'listing-header
  :supertype 'header
  :action (lambda (button)
	    (listing-sort listing-buffer-columns
			  (header-button-label button))))

(defun listing-format-header (columns)
  (let ((len 2))
    (concat "   "
	    (mapconcat
	     (lambda (col)
	       (concat
		(format-header-button (car col)
		 :type 'listing-header
		 'help-echo (concat "mouse-1: Sort by "
				    (downcase (car col))))
		(propertize " "
		 'display `(space :align-to ,(incf len (cadr col)))
		 'face 'fixed-pitch)))
	     columns " "))))

(defun listing-format-element (columns value)
  (let ((elt-len 2)
	(str-len 2)
	(elt-str "  "))
    (while columns
      (let* ((col (pop columns))
	     (val (funcall (caddr col) value))
	     (str (cond ((null val) "-?-")
			((stringp val) val)
			(t (prin1-to-string val)))))
	(incf elt-len (1+ (cadr col)))
	(incf str-len (1+ (length str)))
	(setq elt-str (concat elt-str str))
	(when columns
	  (setq elt-str
		(concat elt-str
			(propertize "\037" 'display
			 (list 'space :width
			       (let ((n (1+ (max 0 (- elt-len str-len)))))
				 (incf str-len n)
				 n))))))))
    (concat elt-str "\n")))

(defun listing-match (predicates value)
  (mapcan (lambda (elt)
	    (let (failed)
	      (while (and predicates (not failed))
		(unless (funcall (pop predicates) elt)
		  (setq failed t)))
	      (unless failed
		(list elt))))
	  value))

(defun listing-insert (columns value)
  (mapc-with-progress-reporter
   "Inserting elements..."
   (lambda (elt)
     (insert (propertize (funcall listing-format-element-function
				  columns elt)
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
	  (setq regexp (concat regexp "\\([^\037]*\\)\037[^\n]*\n")
		columns nil)
	(setq regexp (concat regexp "[^\037]*\037")
	      columns (cdr columns))))
    (sort-regexp-fields (equal column listing-buffer-sort-column)
			regexp "\\1"
			(or from (point-min))
			(or to (point-max)))
    (setq listing-buffer-sort-column
	  (if (equal column listing-buffer-sort-column) nil column))))

(defun listing-line-entered (old new)
  (let ((old-elt (get-text-property old :listing-element))
	(new-elt (get-text-property new :listing-element))
	window buffer)
    (when (and listing-view-element-follow-p
	       (not (eq old-elt new-elt))
	       (not (invisible-p new))
	       (not (bound-and-true-p isearch-mode)))
      (walk-windows (lambda (win)
		      (with-current-buffer (window-buffer win)
			(when (and listing-buffer-element (not window))
			  (setq window win)))))
      (if window
	  ;; Motion hook functions get called twice by design.  In case
	  ;; this is the second time this function is called we don't
	  ;; have to do anything.
	  (unless (equal new-elt (with-current-buffer
				     (setq buffer (window-buffer window))
				   listing-buffer-element))
	    (listing-view-element)
	    (kill-buffer buffer))
	;; Here we can't prevent the message from being shown twice.
	(let ((message-log-max nil))
	  (funcall listing-preview-element-function new-elt))))))

(defun listing-view-element ()
  (interactive)
  (funcall listing-view-element-function
	   (get-text-property (point) :listing-element)))

(defun listing-widen (&optional widen)
  "Remove restrictions (narrowing) from current listing buffer.
This allows all listing elements to be seen."
  (interactive)
  (setq buffer-invisibility-spec nil))

(defvar listing-view-element-follow-p nil)
(make-variable-buffer-local 'listing-view-element-follow-p)

(defvar listing-view-element-function 'ignore)
(make-variable-buffer-local 'listing-view-element-function)

(defvar listing-preview-element-function 'ignore)
(make-variable-buffer-local 'listing-preview-element-function)

(defvar listing-format-element-function 'listing-format-element)
(make-variable-buffer-local 'listing-format-element-function)

(defvar listing-buffer-element nil)
(make-variable-buffer-local 'listing-buffer-element)

(defvar listing-buffer-element-type nil)
(make-variable-buffer-local 'listing-buffer-element-type)

(defvar listing-buffer-columns nil)
(make-variable-buffer-local 'listing-buffer-columns)

(defvar listing-buffer-sort-column nil)
(make-variable-buffer-local 'listing-buffer-sort-column)

(provide 'listing)
;;; listing.el ends here
