;; cucumber.el -- Emacs mode for editing plain text user stories
;;
;; Copyright (C) 2008 Michael Klishin
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; add this to your .emacs to load the mode
;; (add-to-list 'load-path "~/.emacs.d/elisp/cucumber-mode")
;; ;; and load it
;; (autoload 'cucumber-mode "cucumber-mode" "Mode for editing cucumber files" t)
;; (add-to-list 'feature-mode '("\.feature$" . cucumber-mode))

(eval-when-compile (require 'cl))

;;
;; Keywords and font locking
;;

(defconst feature-mode-keywords
  '("Feature" "Scenario", "Given", "Then", "When", "And"))

(cond
 ((featurep 'font-lock)
  (or (boundp 'font-lock-variable-name-face)
      (setq font-lock-variable-name-face font-lock-type-face)))
 (set (make-local-variable 'font-lock-syntax-table) feature-font-lock-syntax-table))

(defconst feature-font-lock-keywords
  (list
   '("^ *Feature:" (0 font-lock-keyword-face) (".*" nil nil (0 font-lock-type-face t)))
   '("^ *Background:$" (0 font-lock-keyword-face))
   '("^ *Scenario\\(?: Outline\\)?:" (0 font-lock-keyword-face) (".*" nil nil (0 font-lock-function-name-face t)))
   '("^ *Given" . font-lock-keyword-face)
   '("^ *When" . font-lock-keyword-face)
   '("^ *Then" . font-lock-keyword-face)
   '("^ *But" . font-lock-keyword-face)
   '("^ *And" . font-lock-keyword-face)
   '("^ *@.*" . font-lock-preprocessor-face)
   '("^ *\\(?:More \\)?Examples:" . font-lock-keyword-face)
   '("^ *#.*" 0 font-lock-comment-face t)
   ))


;;
;; Keymap
;;

(defvar feature-mode-map nil "Keymap used in feature mode")

(if feature-mode-map
    nil
  (setq feature-mode-map (make-sparse-keymap))
  (define-key feature-mode-map "\C-m" 'newline)
  (define-key feature-mode-map "\M-q" 'feature-align-table))

;;
;; Syntax table
;;

(defvar feature-mode-syntax-table nil
  "Syntax table in use in ruby-mode buffers.")

(unless feature-mode-syntax-table
  (setq feature-mode-syntax-table (make-syntax-table)))

;;
;; Variables
;;

(defvar feature-mode-hook nil
  "Hook run when entering `feature-mode'.")

(defcustom feature-indent-level 2
  "Indentation of feature statements"
  :type 'integer :group 'feature)

(defun feature-mode-variables ()
  (set-syntax-table feature-mode-syntax-table)
  (setq require-final-newline t)
  (setq comment-start "# ")
  (setq comment-start-skip "#+ *")
  (setq comment-end "")
  (setq parse-sexp-ignore-comments t)
  (set (make-local-variable 'font-lock-defaults) '((feature-font-lock-keywords) nil nil))
  (set (make-local-variable 'font-lock-keywords) feature-font-lock-keywords))

;;
;; Mode function
;;

;;;###autoload
(defun feature-mode()
  "Major mode for editing plain text stories"
  (interactive)
  (kill-all-local-variables)
  (use-local-map feature-mode-map)
  (setq mode-name "Feature")
  (setq major-mode 'feature-mode)
  (feature-mode-variables)
  (run-mode-hooks 'feature-mode-hook))

(add-to-list 'auto-mode-alist '("\\.feature\\'" . feature-mode))

;;
;; Snippets
;;

(defvar feature-snippet-directory (concat (file-name-directory load-file-name) "snippets")
  "Path to the feature-mode snippets.

If the yasnippet library is loaded, snippets in this directory
are loaded on startup.  If nil, don't load snippets.")

(when (and (featurep 'yasnippet)
           feature-snippet-directory
           (file-exists-p feature-snippet-directory))
  (yas/load-directory feature-snippet-directory))

;;
;; Tables
;;

(defun feature-align-table ()
  "Realign the table at point, if any."
  (interactive)
  (if (feature-in-table-p)
    (save-excursion
      (save-restriction
        (narrow-to-region (feature-beginning-of-table) (feature-end-of-table))
        (feature-indent-table)
        (feature-append-missing-table-bars)
        (feature-pad-table-cells)))))

(defvar feature-table-line-regexp "^[ \t]*|")

(defun feature-in-table-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at feature-table-line-regexp)))

(defun feature-beginning-of-table ()
  (save-excursion
    (beginning-of-line)
    (while (and (looking-at feature-table-line-regexp)
                (not (bobp)))
      (forward-line -1))
    (unless (looking-at feature-table-line-regexp)
      (forward-line))
    (point)))

(defun feature-end-of-table ()
  (save-excursion
    (beginning-of-line)
    (while (looking-at feature-table-line-regexp)
      (or (zerop (forward-line 1))
          (goto-char (point-max))))
    (point)))

(defun feature-each-table-line (line-callback)
  (goto-char (point-min))
  (let ((line-start (make-marker))
        (line-end (make-marker)))
    (while (not (eobp))
      (save-excursion
        (set-marker line-start (point-at-bol))
        (set-marker line-end (point-at-eol))
        (funcall line-callback line-start line-end))
      (forward-line))
    (set-marker line-start nil)
    (set-marker line-end nil)))

(defun feature-each-table-cell (cell-callback)
  (goto-char (point-min))
  (let (cell-column
        (cell-start (make-marker))
        (cell-end (make-marker)))
    (feature-each-table-line
     (lambda (bol eol)
       (goto-char bol)
       (setq cell-column 0)
       (set-marker cell-start (search-forward "|" eol))
       (while (search-forward "|" eol t)
         (set-marker cell-end (1- (point)))
         (save-excursion
           (funcall cell-callback cell-column cell-start cell-end))
         (set-marker cell-start (point))
         (setq cell-column (1+ cell-column)))))
    (set-marker cell-start nil)
    (set-marker cell-end nil)))

(defun feature-indent-table ()
  (let ((indent (save-excursion
                  (goto-char (point-min))
                  (skip-syntax-forward "-"))))
    (feature-each-table-line
     (lambda (s e)
       (goto-char s)
       (delete-horizontal-space)
       (insert (make-string indent ?\ ))))))

(defun feature-append-missing-table-bars ()
  (feature-each-table-line
   (lambda (s e)
     (goto-char e)
     (delete-horizontal-space)
     (if (/= (char-before) ?\|)
         (insert "|")))))

(defun feature-compute-table-column-widths ()
  (let ((widths (make-hash-table))
        width)
    (feature-each-table-cell
     (lambda (column s e)
       (goto-char s)
       (setq s (+ s (skip-syntax-forward "-")))
       (goto-char e)
       (setq e (+ e (skip-syntax-backward "-")))
       (let ((width (max 0 (- e s))))
         (puthash column
                  (max width (gethash column widths 0))
                  widths))))
    widths))

(defun feature-pad-table-cells ()
  (let ((widths (feature-compute-table-column-widths))
        width)
    (feature-each-table-cell
     (lambda (column s e)
       (setq width (gethash column widths 0))
       (unless (and (= (- e s) (+ width 2))
                    (string-match "\\` \\S-.* \\'" (buffer-substring s e)))
         (goto-char s)
         (delete-horizontal-space)
         (insert " ")
         (goto-char e)
         (delete-horizontal-space)
         (insert-before-markers (make-string (- (+ s width 2) (point)) ?\ )))))))
