;;; listing.el --- show columnized view of list elements

;; Copyright (C) 2010  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20100605
;; Updated: 20100811
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
  (buffer-disable-undo)
  (setq truncate-lines t
	buffer-read-only t
	x-stretch-cursor nil
	buffer-invisibility-spec nil))

(defun listing-create (value buffer-or-name columns
			     &optional column mode format)
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
  (with-current-buffer (get-buffer-create buffer-or-name)
    (let ((inhibit-read-only t)
	  (inhibit-point-motion-hooks t))
      (erase-buffer)
      (funcall (or mode 'listing-mode))
      (setq listing-buffer-columns columns)
      (when format
	(setq listing-format-element-function format))
      (listing-insert value)
      (listing-sort column)
      (set-buffer-modified-p nil)
      (listing-align)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;; Local Variables.

;; Local in Listing Buffers.

(defvar listing-view-element-function 'ignore)
(make-variable-buffer-local 'listing-view-element-function)

(defvar listing-preview-element-function 'ignore)
(make-variable-buffer-local 'listing-preview-element-function)

(defvar listing-format-element-function 'listing-format-element)
(make-variable-buffer-local 'listing-format-element-function)

(defvar listing-buffer-columns nil)
(make-variable-buffer-local 'listing-buffer-columns)

(defvar listing-buffer-sort-column nil)
(make-variable-buffer-local 'listing-buffer-sort-column)

(defvar listing-view-buffer-follow-p nil)
(make-variable-buffer-local 'listing-view-buffer-follow-p)

;; Local in View Buffers.

(defvar listing-view-buffer-element nil)
(make-variable-buffer-local 'listing-view-buffer-element)

(defvar listing-view-buffer-element-type nil)
(make-variable-buffer-local 'listing-view-buffer-element-type)

;;; Buttons.

(define-button-type 'listing-header
  :supertype 'header
  :action (lambda (button)
	    (listing-sort (header-button-label button))))

;;; Commands.

(defun listing-view-element ()
  (interactive)
  (funcall listing-view-element-function
	   (get-text-property (point) :listing-element)))

(defun listing-widen (&optional widen)
  "Remove restrictions (narrowing) from current listing buffer.
This allows all listing elements to be seen."
  (interactive)
  (setq buffer-invisibility-spec nil))

;;; List Functions.

(defun listing-insert (value)
  (mapc-with-progress-reporter
   "Inserting elements..."
   (lambda (elt)
     (insert (propertize (funcall listing-format-element-function elt)
			 :listing-element elt
			 'point-entered 'listing-line-entered)))
   value))

(defun listing-sort (column &optional from to)
  (interactive
   (when last-input-event
     (mouse-select-window last-input-event)
     (let ((object (posn-object (event-start last-input-event))))
       (list column (get-text-property (cdr object) 'column-name
				       (car object))))))
  (let ((inhibit-read-only t)
	(inhibit-point-motion-hooks t)
	(columns listing-buffer-columns)
	(regexp "^"))
    (while columns
      (if (equal (caar columns) column)
	  (setq regexp (concat regexp "\\([^\037\n]*\\)\037?[^\n]*\n")
		columns nil)
	(setq regexp (concat regexp "[^\037\n]*\037")
	      columns (cdr columns))))
    (sort-regexp-fields (equal column listing-buffer-sort-column)
			regexp "\\1"
			(or from (point-min))
			(or to (point-max)))
    (setq listing-buffer-sort-column
	  (if (equal column listing-buffer-sort-column) nil column))))

(defun listing-align ()
  (setq header-line-format (listing-format-header))
  (listing-map-lines (lambda (props start end)
		       (listing-align-element))))

(defun listing-categorize (predicate match not)
  (listing-map-lines
   (lambda (props start end)
     (let* ((elt (plist-get props :listing-element))
	    (val (plist-get props 'invisible)))
       (cond ((funcall predicate elt)
	      (when match
		(add-to-list 'val match))
	      (setq val (remove not val)))
	     (t
	      (when match
		(add-to-list 'val not))
	      (setq val (remove match val))))
       (put-text-property start end 'invisible val)))))

(defun listing-map-lines (function &optional regexp subexp)
  (unless subexp
    (setq subexp 0))
  (save-excursion
    (save-restriction
      (let ((inhibit-read-only t)
	    (inhibit-point-motion-hooks t))
	(widen)
	(goto-char (point-min))
	(while (save-excursion
		 (re-search-forward (or regexp "^[^\n]*\n") nil t))
	  (funcall function
		   (text-properties-at (match-beginning subexp))
		   (match-beginning subexp)
		   (match-end subexp))
	  (forward-line))))))

(defun listing-map-elements (function)
  (listing-map-lines
   (lambda (props start end)
     (funcall function (plist-get props :listing-element)))))

;;; Element Functions.

(defun listing-format-element (elt)
  (concat (mapconcat (lambda (col)
		       (let ((val (funcall (caddr col) elt)))
			 (if (stringp val)
			     val
			   (prin1-to-string val))))
		     listing-buffer-columns "\037")
	  "\n"))

(defun listing-format-header ()
  (listing-align-element
   (concat " "
	   (mapconcat (lambda (col)
			(format-header-button
			 (car col) :type 'listing-header
			 'help-echo (concat "mouse-1: Sort by "
					    (downcase (car col)))))
		      listing-buffer-columns "\037"))))

(defun listing-align-element (&optional object)
  (let ((columns listing-buffer-columns)
	(regexp "\\([^\037\n]*\\)\\(\037\\)?")
	(line-length 0)
	(text-length 0)
	(obj-length 1)) ; for header line!
    (while columns
      (if (or (not object) (bufferp object))
	  (re-search-forward regexp (line-end-position) t)
	(string-match regexp object obj-length)
	(incf obj-length (length (match-string 0 object))))
      (let ((col (pop columns))
	    (str (match-string 1 object)))
	(incf line-length (1+ (cadr col)))
	(incf text-length (1+ (length str)))
	(when (match-string 2 object)
	  (put-text-property
	   (match-beginning 2) (match-end 2) 'display
	   (list 'space :width
		 (let ((space (1+ (max 0 (- line-length text-length)))))
		   (incf text-length space)
		   space))
	   object))))
    object))

(defun listing-line-entered (old new)
  (let ((old-elt (get-text-property old :listing-element))
	(new-elt (get-text-property new :listing-element))
	window buffer)
    (when (and listing-view-buffer-follow-p
	       (not (eq old-elt new-elt))
	       (not (invisible-p new))
	       (not (bound-and-true-p isearch-mode)))
      (walk-windows (lambda (win)
		      (with-current-buffer (window-buffer win)
			(when (and listing-view-buffer-element (not window))
			  (setq window win)))))
      (if window
	  ;; Motion hook functions get called twice by design.  In case
	  ;; this is the second time this function is called we don't
	  ;; have to do anything.
	  (unless (equal new-elt (with-current-buffer
				     (setq buffer (window-buffer window))
				   listing-view-buffer-element))
	    (listing-view-element)
	    (kill-buffer buffer))
	;; Here we can't prevent the message from being shown twice.
	(let ((message-log-max nil))
	  (funcall listing-preview-element-function new-elt))))))

(provide 'listing)
;;; listing.el ends here
