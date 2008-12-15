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

(defconst story-mode-keywords
  '("Feature" "Scenario", "Given", "Then", "When", "And"))

(cond
 ((featurep 'font-lock)
  (or (boundp 'font-lock-variable-name-face)
      (setq font-lock-variable-name-face font-lock-type-face)))
 (set (make-local-variable 'font-lock-syntax-table) story-font-lock-syntax-table))

(defconst story-font-lock-keywords
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

(defvar story-mode-map nil "Keymap used in story mode")

(if story-mode-map
    nil
  (setq story-mode-map (make-sparse-keymap))
  (define-key story-mode-map "\C-m" 'newline))

;;
;; Syntax table
;;

(defvar story-mode-syntax-table nil
  "Syntax table in use in ruby-mode buffers.")

(if story-mode-syntax-table
    nil
  (setq story-mode-syntax-table (make-syntax-table)))

;;
;; Variables
;;

(defcustom story-indent-level 2
  "Indentation of story statements"
  :type 'integer :group 'story)

(defun story-mode-variables ()
  (set-syntax-table story-mode-syntax-table)
  (setq require-final-newline t)
  (setq comment-start "# ")
  (setq comment-start-skip "#+ *")  
  (setq comment-end "")
  (setq parse-sexp-ignore-comments t)
  (set (make-local-variable 'font-lock-defaults) '((story-font-lock-keywords) nil nil))
  (set (make-local-variable 'font-lock-keywords) story-font-lock-keywords))

;;
;; Mode function
;;

;;;###autoload
(defun story-mode()
  "Major mode for editing plain text stories"
  (interactive)
  (kill-all-local-variables)
  (use-local-map story-mode-map)
  (setq mode-name "Story")
  (setq major-mode 'story-mode)
  (story-mode-variables))
