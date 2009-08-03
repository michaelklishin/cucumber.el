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
;; (add-to-list 'load-path "~/.emacs.d/elisp/feature-mode")
;; ;; and load it
;; (autoload 'feature-mode "feature-mode" "Mode for editing cucumber files" t)
;; (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;; Key Bindings
;; ------------
;;
;;  \C-c ,v
;;  :   Verify all scenarios in the current buffer file.
;;
;;  \C-c ,s 
;;  :   Verify the scenario under the point in the current buffer.

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
  (define-key feature-mode-map  (kbd "C-c ,s") 'feature-verify-scenario-at-pos)
  (define-key feature-mode-map  (kbd "C-c ,v") 'feature-verify-all-scenarios-in-buffer))

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

(defun feature-minor-modes ()
  "Enable all minor modes for feature mode."
  (turn-on-orgtbl))

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
  (feature-minor-modes)
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
;; Verifying features
;;

(defconst feature-scenario-pattern  "^[[:space:]]*Scenario:[[:space:]]*\\(.*\\)[[:space:]]*$")

(defun feature-scenario-name-at-pos (&optional pos)
  "Returns the name of the scenario at the specified position. if pos is not specified the current buffer location will be used."
  (interactive)
  (let ((start (or pos (point))))
    (save-excursion
      (end-of-line)
      (unless (re-search-backward feature-scenario-pattern nil t)
	(error "Unable to find an scenario"))
      (match-string 1))))

(defun feature-verify-scenario-at-pos (&optional pos)
  "Run the scenario defined at pos.  If post is not specified the current buffer location will be used."
  (interactive)
  (feature-run-cucumber (feature-scenario-names-to-name-opts (cons (feature-scenario-name-at-pos) '()))))

(defun feature-verify-all-scenarios-in-buffer ()
  "Run all the scenarios defined in current buffer."
  (interactive)
  (feature-run-cucumber (feature-scenario-names-to-name-opts (feature-scenario-names-in-buffer))))

(defun feature-run-cucumber (cuke-opts-str)
  "Runs cucumber with the specified options"
  (compile (concat "rake features CUCUMBER_OPTS=\"--no-color" cuke-opts-str "\""))
  (end-of-buffer-other-window 0)
  (with-current-buffer "*compilation*"
    (setq default-directory (feature-project-root))))

(defun feature-scenario-names-in-buffer (&optional so-far)
  "Returns list of all scenario names found in buffer"
  (save-excursion
    (unless so-far 
      (beginning-of-buffer))
    (let ((so-far (or so-far `())))
      (if (re-search-forward feature-scenario-pattern nil t)
	  (feature-scenario-names-in-buffer (cons (match-string 1) so-far))
	so-far))))


(defun feature-scenario-names-to-name-opts (scenario-names &optional opts-str)
  "Build a string that is a series -n options suitable to be pass to cucumber"
  (let ((opts-str (or opts-str "")))
    (if scenario-names
	(concat opts-str " -n \\\"" (car scenario-names) "\\\"" 
		(feature-scenario-names-to-name-opts (cdr scenario-names)))
      opts-str)))
  
(defun feature-root-directory-p (a-directory)
  "Tests if a-directory is the root of the directory tree (i.e. is it '/' on unix)."
  (equal a-directory (rspec-parent-directory a-directory)))

(defun feature-project-root (&optional directory)
  "Finds the root directory of the project by walking the directory tree until it finds a rake file."
  (let ((directory (file-name-as-directory (or directory default-directory))))
    (if (feature-root-directory-p directory) (error "No rakefle found"))
    (if (file-exists-p (concat directory "Rakefile")) 
	directory
      (feature-project-root (file-name-directory (directory-file-name directory))))))



(provide 'cucumber-mode)
(provide 'feature-mode)
