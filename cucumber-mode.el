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
   ;; scenario title
   '("Scenario" . font-lock-function-name-face)
   '("Feature" . font-lock-function-name-face)
   '("Given" . font-lock-keyword-face)
   '("When" . font-lock-keyword-face)
   '("Then" . font-lock-keyword-face)
   '("And" . font-lock-keyword-face)
   ))


;;
;; Keymap
;;

(defvar feature-mode-map nil "Keymap used in feature mode")

(if feature-mode-map
    nil
  (setq feature-mode-map (make-sparse-keymap))
  (define-key feature-mode-map "\C-m" 'newline))

;;
;; Syntax table
;;

(defvar feature-mode-syntax-table nil
  "Syntax table in use in ruby-mode buffers.")

(if feature-mode-syntax-table
    nil
  (setq feature-mode-syntax-table (make-syntax-table)))

;;
;; Variables
;;

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
  (feature-mode-variables))
