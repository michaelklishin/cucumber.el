;; cucumber.el -- Emacs mode for editing plain text user stories
;;
;; Copyright (C) 2008-2010 Michael Klishin and other contributors
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
;;
;; add this to your .emacs to load the mode
;; (add-to-list 'load-path "~/.emacs.d/elisp/feature-mode")
;; ;; and load it
;; (require 'feature-mode)
;; (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
;;
;; Language used in feature file is automatically detected from
;; "language: [2-letter ISO-code]" tag in feature file.  You can choose
;; the language feature-mode should use in case autodetection fails, like this:
;;
;; (defvar feature-default-language "en")
;;
;;
;; Key Bindings
;; ------------
;;
;;  \C-c ,v
;;  :   Verify all scenarios in the current buffer file.
;;
;;  \C-c ,s
;;  :   Verify the scenario under the point in the current buffer.
;;
;;  \C-c ,f
;;  :   Verify all features in project. (Available in feature and
;;      ruby files)
;;
;;  \C-c ,r
;;  :   Repeat the last verification process.

(eval-when-compile (require 'cl))

;;
;; Keywords and font locking
;;

(cond
 ((featurep 'font-lock)
  (or (boundp 'font-lock-variable-name-face)
      (setq font-lock-variable-name-face font-lock-type-face)))
 (set (make-local-variable 'font-lock-syntax-table) feature-font-lock-syntax-table))

(defvar feature-default-language "en")
(defconst feature-keywords-per-language
  '(("ru" . ((feature    . "^ *Функционал:")
             (background . "^ *Предыстория:")
             (scenario   . "^ *Сценари\\(?:й\\|и\\)?\\(?: Структура сценария\\)?:")
             (given    . "^ *Допустим")
             (when   . "^ *Если")
             (then   . "^ *То")
             (but    . "^ *Но")
             (and    . "^ *И\\(?: затем\\)?")
             (examples   . "^ *\\(?:Ещё \\)?Значения:")))
    ("en" . ((feature    . "^ *Feature:")
             (background . "^ *Background:")
             (scenario   . "^ *Scenarios?\\(?: Outline\\)?:")
             (given    . "^ *Given")
             (when   . "^ *When")
             (then   . "^ *Then")
             (but    . "^ *But")
             (and    . "^ *And")
             (examples   . "^ *\\(?:More \\)?Examples:")))
    ("pt" . ((feature    . "^ *Funcionalidade:")
             (background . "^ *Contexto:")
             (scenario   . "^ *Cenário?\\(?: Esquema do Cenário\\)?:")
             (given    . "^ *Dado")
             (when   . "^ *Quando")
             (then   . "^ *Então")
             (but    . "^ *Mas")
             (and    . "^ *E ")
             (examples   . "^ *\\(?:Mais \\)?Exemplos:")))
    ("fi" . ((feature    . "^ *Ominaisuus:")
             (background . "^ *Tausta:")
             (scenario   . "^ *Tapaus\\(?:aihio\\)?:")
             (given    . "^ *Oletetaan")
             (when   . "^ *Kun")
             (then   . "^ *Niin")
             (but    . "^ *Mutta")
             (and    . "^ *Ja")
             (examples   . "^ *Tapaukset:")))
    ("it" . ((feature    . "^ *Funzionalità:")
	     (background . "^ *Contesto:")
	     (scenario   . "^ *Scenario?\\(?: Schema dello scenario\\)?:")
	     (given    . "^ *Dato")
	     (when   . "^ *Quando")
	     (then   . "^ *Allora")
	     (but    . "^ *Ma")
	     (and    . "^ *E")
	     (examples   . "^ *\\(?:Molti \\)?esempi:")))
    ("ja" . ((feature    . "^ *フィーチャ:")
	     (background . "^ *背景:")
	     (scenario   . "^ *シナリオ:")
	     (given    . "^ *前提")
	     (when   . "^ *もし")
	     (then   . "^ *ならば")
	     (but    . "^ *しかし")
	     (and    . "^ *かつ")
	     (examples   . "^ *例:")))))

(defconst feature-font-lock-keywords
  '((feature      (0 font-lock-keyword-face)
                  (".*" nil nil (0 font-lock-type-face t)))
    (background . (0 font-lock-keyword-face))
    (scenario     (0 font-lock-keyword-face)
                  (".*" nil nil (0 font-lock-function-name-face t)))
    (given  . font-lock-keyword-face)
    (when   . font-lock-keyword-face)
    (then   . font-lock-keyword-face)
    (but  . font-lock-keyword-face)
    (and  . font-lock-keyword-face)
    (examples   . font-lock-keyword-face)
    ("^ *@.*"   . font-lock-preprocessor-face)
    ("^ *#.*"     0 font-lock-comment-face t)))


;;
;; Keymap
;;

(defvar feature-mode-map nil "Keymap used in feature mode")

(if feature-mode-map
    nil
  (setq feature-mode-map (make-sparse-keymap))
  (define-key feature-mode-map "\C-m" 'newline)
  (define-key feature-mode-map  (kbd "C-c ,s") 'feature-verify-scenario-at-pos)
  (define-key feature-mode-map  (kbd "C-c ,v") 'feature-verify-all-scenarios-in-buffer)
  (define-key feature-mode-map  (kbd "C-c ,f") 'feature-verify-all-scenarios-in-project))

;; Add relevant feature keybindings to ruby modes
(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c ,f") 'feature-verify-all-scenarios-in-project)))


;;
;; Syntax table
;;

(defvar feature-mode-syntax-table nil
  "Syntax table in use in ruby-mode buffers.")

(unless feature-mode-syntax-table
  (setq feature-mode-syntax-table (make-syntax-table)))

;; Constants

(defconst feature-blank-line-re "^[ \t]*$"
  "Regexp matching a line containing only whitespace.")

(defun feature-feature-re (language)
  (cdr (assoc 'feature (cdr (assoc language feature-keywords-per-language)))))

(defun feature-scenario-re (language)
  (cdr (assoc 'scenario (cdr (assoc language feature-keywords-per-language)))))

;;
;; Variables
;;

(defvar feature-mode-hook nil
  "Hook run when entering `feature-mode'.")

(defcustom feature-indent-level 2
  "Indentation of feature statements"
  :type 'integer :group 'feature)

(defcustom feature-indent-offset 2
  "*Amount of offset per level of indentation."
  :type 'integer :group 'feature)

(defun feature-compute-indentation ()
  "Calculate the maximum sensible indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (if (bobp) 10
      (forward-line -1)
      (while (and (looking-at feature-blank-line-re)
                  (> (point) (point-min)))
        (forward-line -1))
      (+ (current-indentation)
         (if (or (looking-at (feature-feature-re (feature-detect-language)))
                 (looking-at (feature-scenario-re (feature-detect-language))))
             feature-indent-offset 0)))))

(defun feature-indent-line ()
  "Indent the current line.
The first time this command is used, the line will be indented to the
maximum sensible indentation.  Each immediately subsequent usage will
back-dent the line by `feature-indent-offset' spaces.  On reaching column
0, it will cycle back to the maximum sensible indentation."
  (interactive "*")
  (let ((ci (current-indentation))
        (cc (current-column))
        (need (feature-compute-indentation)))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (if (and (equal last-command this-command) (/= ci 0))
          (indent-to (* (/ (- ci 1) feature-indent-offset) feature-indent-offset))
        (indent-to need)))
    (if (< (current-column) (current-indentation))
        (forward-to-indentation 0))))

(defun feature-font-lock-keywords-for (language)
  (let ((result-keywords . ()))
    (dolist (pair feature-font-lock-keywords)
      (let* ((keyword (car pair))
             (font-locking (cdr pair))
             (language-keyword (cdr (assoc keyword
                                           (cdr (assoc
                                                 language
                                                 feature-keywords-per-language))))))

        (push (cons (or language-keyword keyword) font-locking) result-keywords)))
    result-keywords))

(defun feature-detect-language ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "language: \\([[:alpha:]-]+\\)"
                           (line-end-position)
                           t)
        (match-string 1)
      feature-default-language)))

(defun feature-mode-variables ()
  (set-syntax-table feature-mode-syntax-table)
  (setq require-final-newline t)
  (setq comment-start "# ")
  (setq comment-start-skip "#+ *")
  (setq comment-end "")
  (setq parse-sexp-ignore-comments t)
  (set (make-local-variable 'indent-tabs-mode) 'nil)
  (set (make-local-variable 'indent-line-function) 'feature-indent-line)
  (set (make-local-variable 'font-lock-defaults)
       (list (feature-font-lock-keywords-for (feature-detect-language)) nil nil))
  (set (make-local-variable 'font-lock-keywords)
       (feature-font-lock-keywords-for (feature-detect-language))))

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

(defun feature-scenario-name-re (language)
  (concat (feature-scenario-re (feature-detect-language)) "[[:space:]]+\\(.*\\)$"))

(defun feature-scenario-name-at-pos (&optional pos)
  "Returns the name of the scenario at the specified position. if pos is not specified the current buffer location will be used."
  (interactive)
  (let ((start (or pos (point))))
    (save-excursion
      (end-of-line)
      (unless (re-search-backward (feature-scenario-name-re (feature-detect-language)) nil t)
        (error "Unable to find an scenario"))
      (match-string-no-properties 1))))

(defun feature-verify-scenario-at-pos (&optional pos)
  "Run the scenario defined at pos.  If post is not specified the current buffer location will be used."
  (interactive)
  (feature-run-cucumber
   (list "-n" (concat "'" (feature-escape-scenario-name (feature-scenario-name-at-pos)) "'"))
   :feature-file (buffer-file-name)))

(defun feature-verify-all-scenarios-in-buffer ()
  "Run all the scenarios defined in current buffer."
  (interactive)
  (feature-run-cucumber '() :feature-file (buffer-file-name)))


(defun feature-verify-all-scenarios-in-project ()
  "Run all the scenarios defined in current project."
  (interactive)
  (feature-run-cucumber '()))

(defun feature-register-verify-redo (redoer)
  "Register a bit of code that will repeat a verification process"
  (let ((redoer-cmd (eval (list 'lambda ()
                                '(interactive)
                                (list 'let (list (list `default-directory
                                                       default-directory))
                                      redoer)))))

    (global-set-key (kbd "C-c ,r") redoer-cmd)))

(defun feature-run-cucumber (cuke-opts &optional &key feature-file)
  "Runs cucumber with the specified options"
  (feature-register-verify-redo (list 'feature-run-cucumber
                                      (list 'quote cuke-opts)
                                      :feature-file feature-file))
  ;; redoer is registered

  (let ((opts-str    (mapconcat 'identity cuke-opts " "))
        (feature-arg (if feature-file
                         (concat " FEATURE='" feature-file "'")
                       "")))
    (ansi-color-for-comint-mode-on)
    (compile (concat "rake cucumber CUCUMBER_OPTS=\"" opts-str "\"" feature-arg) t))
  (end-of-buffer-other-window 0))

(defun feature-escape-scenario-name (scenario-name)
  "Escapes all the characaters in a scenario name that mess up using in the -n options"
  (replace-regexp-in-string "\\(\"\\)" "\\\\\\\\\\\\\\1" (replace-regexp-in-string "\\([()\']\\|\\[\\|\\]\\)" "\\\\\\1" scenario-name)))

(defun feature-root-directory-p (a-directory)
  "Tests if a-directory is the root of the directory tree (i.e. is it '/' on unix)."
  (equal a-directory (rspec-parent-directory a-directory)))

(defun feature-project-root (&optional directory)
  "Finds the root directory of the project by walking the directory tree until it finds Rakefile (presumably, application root)"
  (let ((directory (file-name-as-directory (or directory default-directory))))
    (if (feature-root-directory-p directory) (error "No rakefle found"))
    (if (file-exists-p (concat directory "Rakefile"))
        directory
      (feature-project-root (file-name-directory (directory-file-name directory))))))



(provide 'cucumber-mode)
(provide 'feature-mode)
