;;; listing.el --- show columnized view of list elements

;; Copyright (C) 2010  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20100605
;; Updated: 20100816
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
			     &optional column mode nosort)
  "Insert elements of the list VALUE into BUFFER, one per line.

MODE is the major mode used in BUFFER.  If it is nil `listing-mode' is
used.

COLUMN is the column by which the elements are sorted initialy.  COLUMNS
specifies what parts are inserted and can optionally be used to format
the output of each column individually. It has the form:

  ((HEADER LENGTH VISIBLE ACCESSOR)...)

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
      (setq listing-buffer-columns
	    (mapcar (lambda (column)
		      (unless (caddr column)
			(add-to-invisibility-spec
			 (listing-column-symbol column)))
		      (copy-list column))
		    columns))
      (listing-insert value)
      (unless nosort
	(listing-sort column))
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

(defvar listing-element-font-function nil)
(make-variable-buffer-local 'listing-element-font-function)

(defvar listing-invisibility-setup nil)
(make-variable-buffer-local 'listing-invisibility-setup)

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
	   (get-text-property (point) 'listing-element)))

(defun listing-widen (&optional widen)
  "Remove restrictions (narrowing) from current listing buffer.
This allows all listing elements to be seen."
  (interactive)
  (setq buffer-invisibility-spec nil))

(defun listing-hide-column (column)
  (interactive
   (list (completing-read "Column: "
			  (mapcan (lambda (col)
				    (when (caddr col)
				      (list (car col))))
				  listing-buffer-columns)
			  nil t)))
  (add-to-invisibility-spec (listing-column-symbol column))
  (setf (caddr (assoc column listing-buffer-columns)) nil)
  (listing-align))

(defun listing-show-column (column)
  (interactive
   (list (completing-read "Column: "
			  (mapcan (lambda (col)
				    (unless (caddr col)
				      (list (car col))))
				  listing-buffer-columns)
			  nil t)))
  (remove-from-invisibility-spec (listing-column-symbol column))
  (setf (caddr (assoc column listing-buffer-columns)) t)
  (listing-align))

;;; List Functions.

(defun listing-insert (value)
  (mapc-with-progress-reporter "Inserting elements..."
			       'listing-insert-element value))

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
    (setq regexp (concat regexp "\\(\037[^\037\n]*\n\\)*"))
    (sort-regexp-fields (equal column listing-buffer-sort-column)
			regexp "\\1"
			(or from (point-min))
			(or to (point-max)))
    (setq listing-buffer-sort-column
	  (if (equal column listing-buffer-sort-column) nil column))))

(defun listing-align ()
  (setq header-line-format (listing-format-header))
  (listing-map-lines
   "Aligning columns..."
   (lambda (props start end)
     (listing-align-element))))

(defun listing-map-lines (message function &optional regexp subexp)
  (unless subexp
    (setq subexp 0))
  (save-excursion
    (save-restriction
      (widen)
      (let ((inhibit-read-only t)
	    (inhibit-point-motion-hooks t)
	    (idx 0)
	    (progress (make-progress-reporter
		       message 0 (count-lines (point-min) (point-max)))))
	(goto-char (point-min))
	(while (save-excursion
		 (re-search-forward (or regexp "^[^\n]*\n") nil t))
	  (funcall function
		   (text-properties-at (match-beginning subexp))
		   (match-beginning subexp)
		   (match-end subexp))
	  (forward-line)
	  (progress-reporter-update progress (incf idx)))
	(progress-reporter-done progress)))))

(defun listing-map-elements (message function)
  (listing-map-lines
   message
   (lambda (props start end)
     (funcall function (plist-get props 'listing-element)))))

;;; Row Functions.

(defun listing-insert-element (elt)
  (let ((face (when listing-element-font-function
		(funcall listing-element-font-function elt)))
	(invisible (mapcan (lambda (fn)
			     (funcall fn elt))
			   listing-invisibility-setup))
	(columns listing-buffer-columns))
    (while columns
      (let* ((column (pop columns))
	     (colsym (listing-column-symbol column))
	     (value  (funcall (nth 3 column) elt))
	     (string (if (stringp value)
			 (copy-sequence value)
		       (prin1-to-string value))))
	(insert
	 (propertize
	  (concat (propertize
		   (concat string (when columns "\037"))
		   'face (or (get-text-property 0 'face string) face)
		   'invisible (cons colsym invisible))
		  (unless columns
		    (propertize "\n" 'invisible invisible)))
	  'listing-element elt
	  'point-entered 'listing-line-entered))))))

(defun listing-format-header ()
  (let (text (columns listing-buffer-columns))
    (while columns
      (let* ((column (pop columns))
	     (header (car column)))
	(when (caddr column)
	  (setq text (concat text
			     (format-header-button
			      header :type 'listing-header
			      'help-echo (concat "mouse-1: Sort by "
						 (downcase header)))
			     (when columns "\037"))))))
    (listing-align-element (concat " " text))))

(defun listing-align-element (&optional header)
  (let ((columns listing-buffer-columns)
	(regexp "\\([^\037\n]*\\)\\(\037\\)?")
	(line-length 0)
	(text-length 0)
	(head-length 1))
    (while columns
      (let ((column (pop columns)))
	(unless header
	  (re-search-forward regexp (line-end-position) t))
	(when (caddr column)
	  (when header
	    (string-match regexp header head-length)
	    (incf head-length (length (match-string 0 header))))
	  (let ((str (match-string 1 header)))
	    (incf line-length (cadr column))
	    (incf text-length (length str))
	    (when (match-string 2 header)
	      (put-text-property
	       (match-beginning 2) (match-end 2) 'display
	       (list 'space :width
		     (let ((space (1+ (max 0 (- line-length text-length)))))
		       (incf text-length space)
		       (incf line-length)
		       space))
	       header))))))
    header))

(defun listing-line-entered (old new)
  (let ((old-elt (get-text-property old 'listing-element))
	(new-elt (get-text-property new 'listing-element))
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

;;; Utitlity Functions.

(defun listing-column-symbol (column)
  (intern (concat "column:" (downcase (if (listp column)
					  (car column)
					column)))))

(defun listing-add-to-invisibile-prop (invisible)
  (let ((pos (point))
	(end (line-end-position)))
    (while (<= pos end)
      (let ((old (get-text-property pos 'invisible)))
	(put-text-property pos (1+ pos) 'invisible
			   (if (listp invisible)
			       (nconc invisible old)
			     (cons invisible old))))
      (incf pos))))

(provide 'listing)
;;; listing.el ends here
