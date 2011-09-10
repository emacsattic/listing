;;; listing.el --- show columnized view of list elements

;; Copyright (C) 2010-2011  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20100605
;; Version: 0.2.1-git
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

;; This library provides function `listing-create' which inserts the
;; elements in a list into a buffer, one per row.  Columns are used to
;; display different attributes of the elements.

;; How a cell's values is calculated and formatted is not restricted by
;; this package in any way.

;; In other words this library can be used by other libraries to easily
;; create tables similar to the one created by `buffer-menu' but for
;; arbitrary objects and without much effort.

;; A very basic example:
;;
;;   (listing-create '((A . a) (B . b) (C . c))
;;                   (get-buffer-create "*listing*")
;;                   '(("upper" 5 t car)
;;                     ("lower" 5 t cdr)))

;; But it doesn't stop here.  See the function `listing-create' and
;; variable `listing-view-element-function' for more information.  This
;; library was created for library `epkg.el', which serves well as an
;; example.

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
  (make-local-variable 'search-invisible)
  (make-local-variable 'isearch-filter-predicate)
  (add-hook 'isearch-mode-end-hook 'listing-isearch-end nil t)
  (setq truncate-lines t
	buffer-read-only t
	x-stretch-cursor nil
	buffer-invisibility-spec nil
	search-invisible nil
	isearch-filter-predicate (lambda (pos d)
				   (not (invisible-p pos)))))

(defun listing-create (value buffer-or-name columns
			     &optional column mode nosort)
  "Insert elements of the list VALUE into BUFFER, one per row.

Optional MODE if specified should be a mode deriving from `listing-mode';
it is used as the major mode of BUFFER.  Otherwise `listing-mode' is used.

COLUMN is the column by which the elements are sorted initially.  COLUMNS
specifies the columns used in BUFFER and how cell strings are extracted
and formatted.

  ((HEADER LENGTH VISIBLE ACCESSOR)...)

HEADER is a string used as column label in the header line.  ACCESSOR is
a function used to extract and format cells in the column.  It is called
once for each element with the element as argument; the string it returns
is inserted into the respective cell.  LENGTH defines the minimal length
of the cells in column. ACCESSOR does not have to take care of alignment.

The behavior of the listing created by this function can be further
customized by setting various buffer local variables using the mode
function MODE."
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

;;; Buffer Local Variables.

(defvar listing-view-element-function nil
  "The function used to view an element in a separate buffer.

This function is called by command `listing-view-element' to view details
about the current element in a buffer different from the listing buffer.
The buffer should be displayed in the same frame as the listing buffer but
should not reuse it's window.  Otherwise the element buffer can not be
automatically updated resp. replaced as described below.

This function is called with the current ELEMENT and a BUFFER as arguments.
If BUFFER uses the mode specified by `listing-view-element-mode' then this
function should use it to display ELEMENT or replace it with another
buffer, whichever is suitable for the mode used in that buffer.

When `listing-view-element' is called interactively and the selected frame
does not display a element buffer then BUFFER is the listing buffer.  This
function should then still create or make visible a element buffer in the
selected frame.

When `listing-view-element-follow-p' is non-nil this function is also
called by function `listing-line-entered' whenever point moves to another
element.  In this case this function is called twice which is due to the
way motion hooks are implemented in Emacs; there is not much that can be
done about it.

This function should therefor setup a buffer local variable which allows
it to determine whether it is being called the second time and in such a
case just abort.

Finally when variable `listing-preview-element' is t `listing-line-entered'
calls this function without the BUFFER argument if no window of the
selected frame displays a listing buffer.  See `listing-preview-element'
for information about the expected behavior in this situation.

See function `epkg-describe-line-package' defined in library `epkg.el'
for an example demonstrating that just takes a few lines and is less
complicated than it might sound.")
(make-variable-buffer-local 'listing-view-element-function)

(defvar listing-view-element-follow-p nil
  "Whether the element buffer should be updated on movement.
See variable `listing-view-element-function' for more information.")
(make-variable-buffer-local 'listing-view-element-follow-p)

(defvar listing-view-element-mode nil
  "The major mode used in element buffers.
The function specified by variable `listing-view-element-function' (which
see) needs this to be set.")
(make-variable-buffer-local 'listing-view-element-mode)

(defvar listing-preview-element nil
  "The function used to echo information about an element.
If this is nil do not display information in the echo area.  If it is t
use the function specified by variable `listing-view-element-function'
\(which see) instead.")
(make-variable-buffer-local 'listing-preview-element-function)

(defvar listing-element-font-function nil
  "The function used to select the font to propertize element rows.
This function is called for each element with the element as argument.")
(make-variable-buffer-local 'listing-element-font-function)

(defvar listing-invisibility-setup nil
  "The function used to set the `invisible' property of element rows.
This function is called for each element with the element as argument.")
(make-variable-buffer-local 'listing-invisibility-setup)

(defvar listing-buffer-columns nil
  "The columns used do display elements.
This is a copy of the value passed to `listing-create' as COLUMNS, and is
later changed to track state.")
(make-variable-buffer-local 'listing-buffer-columns)

(defvar listing-buffer-sort-column nil
  "The columns by which elements are currently sorted.
The initial value is set using the COLUMN argument of `listing-create'.")
(make-variable-buffer-local 'listing-buffer-sort-column)

;;; User Commands.

(defun listing-view-element (element &optional buffer)
  "In Listing Buffers; view ELEMENT in a buffer.
Interactively view the element on the current row."
  (interactive (list (listing-line-element)
		     (or (listing-element-buffer)
			 (current-buffer))))
  (funcall listing-view-element-function element buffer))

(defun listing-widen (&optional symbol)
  "In Listing Buffers; make hidden elements visible again.
Interactively prompt for a narrowing to be undone.  Only hidden rows
can be made visible; to unhide columns use `listing-show-column'."
  (interactive
   (let ((spec (listing-rows-invisibility-spec)))
     (unless (or current-prefix-arg (= (length spec) 1))
       (list (intern (completing-read "Unhide elements matching: "
				      spec nil t))))))
  (if symbol
      (remove-from-invisibility-spec symbol)
    (dolist (string (listing-rows-invisibility-spec))
      (remove-from-invisibility-spec (intern string)))))

(defun listing-hide-column (column)
  "In Listing Buffers; hide COLUMN.
Interactively prompt for the column to be hidden.  COLUMN is a string
which also has to be a key in the alist `listing-buffer-columns'."
  (interactive
   (list (completing-read "Hide column: "
			  (listing-columns-invisibility-spec t) nil t)))
  (add-to-invisibility-spec (listing-column-symbol column))
  (setf (caddr (assoc column listing-buffer-columns)) nil)
  (listing-align))

(defun listing-show-column (column)
  "In Listing Buffers; unhide COLUMN.
Interactively prompt for the column to be unhidden.  COLUMN is a string
which also has to be a key in the alist `listing-buffer-columns'."
  (interactive
   (list (completing-read "Show column: "
			  (listing-columns-invisibility-spec) nil t)))
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

(defmacro listing-map-elements (message function)
  `(listing-map-lines
    ,message
    (lambda (props start end)
      (funcall ,function (plist-get props 'listing-element)))))

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
	     (value  (funcall (nth 3 column) elt (butlast column)))
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
	  'point-entered (when listing-view-element-function
			   'listing-line-entered)))))))

(define-button-type 'listing-header
  :supertype 'header
  :action (lambda (button)
	    (listing-sort (header-button-label button))))

(defun listing-format-header ()
  (let (text (columns listing-buffer-columns))
    (while columns
      (let* ((column (pop columns))
	     (header (car column)))
	(when (caddr column)
	  (setq text (concat text
			     (header-button-format
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
  (when (eq new (point-max))
    (goto-char old))
  (let ((old-elt (listing-line-element old))
	(new-elt (listing-line-element new)))
    (when (and listing-view-element-follow-p
	       (not (eq old-elt new-elt))
	       (not (invisible-p new))
	       (not (bound-and-true-p isearch-mode)))
      (let ((buffer (listing-element-buffer)))
	(cond (buffer
	       (listing-view-element new-elt buffer))
	      ((eq listing-preview-element t)
	       (listing-view-element new-elt))
	      (listing-preview-element
	       (funcall listing-preview-element new-elt)))))))

;;; Utility Functions.

(defun listing-line-element (&optional pos)
  (get-text-property (or pos (point)) 'listing-element))

(defun listing-column-symbol (column)
  (intern (concat "column:" (downcase (if (listp column)
					  (car column)
					column)))))

(defun listing-isearch-end ()
  (unless isearch-mode-end-hook-quit
    (listing-line-entered isearch-opoint (point))))

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

(defun listing-rows-invisibility-spec ()
  (mapcan (lambda (elt)
	    (setq elt (symbol-name elt))
	    (unless (string-match "^column:" elt)
	      (list elt)))
	  buffer-invisibility-spec))

(defun listing-columns-invisibility-spec (&optional reverse)
  (mapcan (lambda (col)
	    (when (if reverse (caddr col) (not (caddr col)))
	      (list (car col))))
	  listing-buffer-columns))

(defun listing-element-buffer ()
  (let (window (mode listing-view-element-mode))
    (walk-windows (lambda (win)
		    (with-current-buffer (window-buffer win)
		      (when (and (not window) (eq major-mode mode))
			(setq window win)))))
    (when window
      (window-buffer window))))

(provide 'listing)
;;; listing.el ends here
