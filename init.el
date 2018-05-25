;;; init.el --- Alexey Shiklomanov's Emacs init file
;;;
;;; Commentary:
;;;
;;;
;;; Code:
;; Use Emacs's internal package manager
(package-initialize)
(require 'package)
(setq package-enable-at-startup nil)

;; Load some common package repositories. MELPA is the big one.
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

;; Custom file. Mostly, I avoid using custom in favor of ~setq~.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Also load stuff from emacswiki
(add-to-list 'load-path (expand-file-name "emacswiki" user-emacs-directory))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; Global settings
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      show-paren-delay 0
      abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory)
      save-abbrevs 'silent)
(show-paren-mode 1)
(tool-bar-mode -1)
(electric-pair-mode 1)		; Auto-close braces, parentheses, etc.

(defvar backup-dir (expand-file-name "backups" user-emacs-directory))
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)

;; I want line numbers for programming (prog) and text modes
(defun ans-prog-mode-setup ()
  "My custom setup for prog mode."
  (linum-mode 1)
  (toggle-truncate-lines 1)
  (flyspell-prog-mode))
(defun ans-text-mode-setup ()
  "My custom configuration for text mode."
  (linum-mode 1)
  (visual-line-mode)
  (flyspell-mode))
(add-hook 'prog-mode-hook #'ans-prog-mode-setup)
(add-hook 'text-mode-hook #'ans-text-mode-setup)

;; Set the current theme
(load-theme 'wombat)

;; General package for better key-bindings
(use-package general
  :ensure t)

;; Turn on evil mode by default
(use-package evil
  :ensure t
  :config
  (evil-mode)
  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))
  (use-package evil-indent-textobject
    :ensure t)
  (add-to-list 'evil-emacs-state-modes 'dired-mode)
  ;; Don't use evil mode in these, but enable some Evil mappings
  (add-to-list 'evil-emacs-state-modes 'help-mode)
  (add-to-list 'evil-emacs-state-modes 'messages-buffer-mode)
  (add-to-list 'evil-emacs-state-modes 'special-mode)
  (general-def
    :keymaps '(help-mode-map
	       messages-buffer-mode-map
	       special-mode-map)
    "h" 'evil-backward-char
    "l" 'evil-forward-char
    "k" 'evil-previous-visual-line
    "j" 'evil-next-visual-line
    "C-u" 'evil-scroll-up
    "C-d" 'evil-scroll-down
    "/" 'evil-search-forward
    "n" 'evil-search-next
    "N" 'evil-search-previous
    )
  )

(general-unbind '(motion normal visual)
  "SPC"
  "C-u"
  "\\")

(general-def
  :states 'insert
  "j" (general-key-dispatch 'self-insert-command
	:timeout 0.25
	"k" 'evil-normal-state)
  )

(general-def
  :states '(motion normal emacs)
  ;; Move by visual lines
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line
  "gj" 'evil-next-line
  "gk" 'evil-previous-line
  "C-=" 'evil-window-increase-height
  "C--" 'evil-window-decrease-height
  "C-+" 'evil-window-increase-width
  "C-_" 'evil-window-decrease-width
  "C-0" 'balance-windows
  "C-)" 'shrink-window-if-larger-than-buffer
  "<C-M-down>" 'evil-window-increase-height
  "C-d" 'evil-scroll-down
  "C-u" 'evil-scroll-up
  )

(general-def
  :states 'normal
  "S" 'save-buffer
  )

(general-create-definer ans-leader-def :prefix "SPC")
(ans-leader-def
  :states '(motion normal emacs)
  :keymaps 'override
  "b" 'helm-mini
  "o" 'other-window
  "0" 'delete-other-windows
  "\\" 'evil-window-vsplit
  "-" 'evil-window-split
  "<up>" 'buf-move-up
  "<down>" 'buf-move-down
  "<left>" 'buf-move-left
  "<right>" 'buf-move-right
  "O" 'other-frame
  "E" 'new-frame
  "TAB" 'ans-switch-to-mru-buffer
  "." 'evil-next-buffer
  "," 'evil-prev-buffer
  ":" 'eval-expression
  "d" 'dired
  "x" 'helm-M-x
  "q" 'kill-this-buffer
  "Q" 'evil-quit
  "sv" 'ans--reload-initfile
  "ev" 'ans--edit-initfile
  "sx" (lambda() (interactive)(switch-to-buffer "*scratch*"))
  "ws" 'toggle-truncate-lines
  "ss" 'delete-trailing-whitespace
  "L" 'whitespace-mode			; Show all whitespace characters
  "z" 'ans-toggle-minimize
  "'" 'comment-dwim			; Insert right comment
  "ea" 'align-regexp
  "*" 'universal-argument		; Emacs's C-u
  )

(defvar ans-window-minimized '()
  "Configuration of currently minimized windows.
See `ans-toggle-minimize'.")

(defun ans-toggle-minimize ()
  "Toggle the maximization state of a window."
  (interactive)
  (if ans-window-minimized
      (progn (set-window-configuration (pop ans-window-minimized))
	     (message "Windows restored."))
    (progn (push (current-window-configuration) ans-window-minimized)
	   (delete-other-windows)
	   (message "Window minimized."))
    ))

;; Edit comment boxes.
(use-package rebox2
  :ensure t
  :general
  (general-def
    :states '(normal insert)
    "M-Q" 'rebox-mode
    "M-q" 'rebox-dwim)
  :config
  (add-to-list 'rebox-language-character-alist '(7 . "!"))
  (setq rebox-regexp-start (vconcat rebox-regexp-start '("^[ \t]*!+")))
  (rebox-register-all-templates)
  )

(general-def
  :keymaps 'lisp-mode-shared-map
  :states '(motion insert)
  "<C-return>" 'eval-defun)

;; Swap buffer positions
(use-package buffer-move :ensure t)

(use-package magit
  :ensure t
  :general
  (ans-leader-def
    :states 'normal
    "g s" 'magit-status)
  )
(use-package evil-magit :ensure t)

(use-package diff-hl
  :ensure t
  :general
  (general-def
    :states 'normal
    "] c" 'diff-hl-next-hunk
    "[ c" 'diff-hl-previous-hunk)
  (ans-leader-def
    :states 'normal
    "g g" 'diff-hl-mode))

(use-package diminish :ensure t)
(use-package helm
  :ensure t
  :diminish helm-mode
  :demand
  :init
  (require 'helm-config)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 20)
  (setq helm-mode-fuzzy-match t)
  (setq helm-grep-ag-command
	"rg --color=always --smart-case --no-heading --line-number %s %s %s")
  (setq helm-autoresize-max-height 20)
  (setq helm-display-function 'helm-display-buffer-in-own-frame
	helm-display-buffer-reuse-frame t
	helm-use-undecorated-frame-option t)
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)
  :general
  (ans-leader-def
    :states 'normal
    "t" 'helm-find-files)
  )

(use-package projectile
  :ensure t
  :commands (projectile-mode)
  :config
  (projectile-mode))

(use-package helm-projectile
  :ensure t
  :init
  (setq helm-projectile-fuzzy-match t)
  :general
  (ans-leader-def
    :states 'normal
    "P" 'helm-projectile
    "f" 'helm-projectile-find-file
    "p" 'helm-projectile-switch-project
    )
  )

;; (use-package fuzzy :ensure t)
(use-package evil-nerd-commenter
  :ensure t
  :general
  (ans-leader-def
    :states '(normal visual)
    ";" 'evilnc-comment-or-uncomment-lines
    "\"" 'evilnc-comment-or-uncomment-paragraphs
    )
  )

(use-package company
  :ensure t
  :commands global-company-mode
  :diminish 'company-mode
  :init
  (setq company-selection-wrap-around t)
  (setq company-idle-delay nil)
  :config
  (global-company-mode)
  ;; Thanks to this:
  ;; https://github.com/otijhuis/evil-emacs.d/blob/7c122b0e05c367192444a85d12323487422b793b/config/evil-settings.el#L38-L39
  (add-hook 'evil-insert-state-exit-hook (lambda ()(company-abort)))
  :general
  (general-def
    :states 'insert
    ;; "TAB" 'company-indent-or-complete-common
    ;; See below for discussion of company-dabbrev-code
    ;; https://github.com/company-mode/company-mode/issues/360
    "C-n" 'company-dabbrev-code
    "C-p" 'company-dabbrev-code
    "C-f" 'ans-company-current-directory-backend
    "C-l" 'company-complete		; Note that this includes company-files
    )
  (general-def
    :keymaps 'company-active-map
    "ESC" 'company-abort
    "TAB" 'company-complete-common
    "C-l" 'ans-company-complete-continue
    "SPC" 'company-complete-common-or-cycle
    "C-n" 'company-select-next
    "C-p" 'company-select-previous)
  )

(defun ans-company-complete-continue ()
  "Insert the result of a completion, then re-start completion.
This makes repeat completions easier (e.g. when completing long file paths)."
  (interactive)
  (company-complete-selection)
  (company-complete))

(defun ans-company-current-directory-backend (command &optional arg &rest ignored)
  "Complete files in the current directory.
COMMAND company completion command.
ARG company completion arguments.
IGNORED company ignored arguments."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'ans-company-current-directory-backend))
    (prefix (company-grab-symbol))
    (candidates
     (remove-if-not
      (lambda (c) (string-prefix-p arg c))
      (directory-files ".")))
    ))

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :init
  (setq org-todo-keywords
	'((sequence "TODO" "STARTED" "VERIFY" "|" "DONE" "CANCELED")))
  (setq org-capture-templates
	'(("E" "Emacs config"
	   entry (file+headline "~/Dropbox/Notes/emacs.org" "Configuration to-do list")
	   "** TODO %?")
	  ("e" "Emacs note"
	   entry (file+headline "~/Dropbox/Notes/emacs.org" "Misc")
	   "** %?")))
  (add-hook 'org-mode-hook (lambda () (linum-mode -1)))
  :general
  (general-def
    :states '(motion normal)
    :keymaps 'org-mode-map
    "<backspace>" 'outline-hide-subtree)
  (ans-leader-def
    :states '(motion normal emacs)
    "C" 'org-capture
    "#" 'org-update-statistics-cookies
    "g t" 'org-todo))

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode) . rainbow-delimiters-mode))

(use-package ace-jump-mode
  :ensure t
  :general
  (ans-leader-def
    :states '(motion normal emacs)
    "SPC s" 'evil-ace-jump-char-mode
    "SPC w" 'evil-ace-jump-word-mode
    "SPC l" 'evil-ace-jump-line-mode))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  (general-def
    :states '(motion normal)
    "]a" 'flycheck-next-error
    "[a" 'flycheck-previous-error
    "]A" 'flycheck-first-error)
  (ans-leader-def
    :states '(motion normal)
    "!" 'flycheck-list-errors)
  )

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook (lambda () (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Emacs Speaks Statistics (ESS)
;; IDE for R and other stats programs
(use-package ess
  :ensure t
  :init
  (add-to-list 'evil-emacs-state-modes 'inferior-ess-mode)
  (setq ess-ask-for-ess-directory nil)
  (setq ess-directory-function 'ans-r-file-here)
  (setq ess-default-style 'RStudio)
  (setq ess-use-company t)
  :general
  (:keymaps 'ess-mode-map
	    :states 'normal
	    :prefix "\\"
	    "r f" 'ans-start-R
	    "r q" 'ans-quit-R
	    "l" 'ess-eval-line
	    "d" 'ess-eval-line-and-step
	    "p p" 'ess-eval-paragraph
	    "p d" 'ess-eval-paragraph-and-step
	    "a a" 'ess-eval-buffer
	    "a d" 'ess-eval-buffer-from-here-to-end
	    "a s" 'ess-eval-buffer-from-beg-to-here)
  (:states 'visual
	   :keymaps 'ess-mode-map
	   :prefix "\\"
	   "s s" 'ess-eval-region)
  (:states 'insert
	   :keymaps 'ess-mode-map
	   "M-m" (lambda() (interactive)(insert " %>%"))
	   "M--" 'ess-insert-S-assign
	   "C-c" (lambda() (interactive)(insert "#'")))
  )

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.Rmd\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc"))

(use-package mmm-mode
  :ensure t
  :demand
  :init
  (setq mmm-global-mode 'maybe)
  (setq mmm-submode-decoration-level 1)
  (setq mmm-parse-when-idle t)
  (setq mmm-idle-timer-delay 0.2)
  :config
  (mmm-add-classes
   '((ans-rmarkdown
      :submode R-mode
      :face mmm-declaration-submode-face
      :front "^```{r.*}[\r\n]"
      :back "^```$"
      ))
   )
  (mmm-add-mode-ext-class 'markdown-mode "\\.Rmd\\'" 'ans-rmarkdown)
  )

(defun ans-latex-mode-setup ()
  "Set custom options for LaTeX files."
  (require 'reftex)
  ;; Use settings for text mode
  (ans-text-mode-setup)
  ;; Use the "default" vim paragraph definition
  (setq paragraph-start "\f\\|[ 	]*$")
  (setq paragraph-separate "[ 	\f]*$")
  )

(use-package tex
  :defer t
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'ans-latex-mode-setup)
  :config
  (use-package auctex-latexmk
    :ensure t
    :init
    (setq auctex-latexmk-inherit-TeX-PDF-mode t)
    :config
    (auctex-latexmk-setup))
  )

(use-package simpleclip
  :ensure t
  :config
  (simpleclip-mode 1))

(use-package yasnippet
  :ensure t
  :demand
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"
			   "~/.emacs.d/remote-snippets"))
  :config
  (yas-reload-all)
  (yas-global-mode 1)
  ;; Use something like this if you don't want snippets globally:
  ;; (yas-reload-all)
  ;; (add-hook 'prog-mode-hook #'yas-minor-mode)
  :general
  (general-def
    :keymaps 'yas-minor-mode-map
    "<escape>" 'yas-exit-snippet)
  (ans-leader-def
    :states '(motion normal)
    "un" 'yas-new-snippet
    "ue" 'yas-visit-snippet-file)
  )

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  :general
  (general-def
    :states 'insert
    "M-e" 'sp-forward-slurp-sexp
    "M-w" 'sp-forward-barf-sexp))

(use-package helm-bibtex
  :ensure t
  :commands helm-bibtex)
(evil-ex-define-cmd "bib[tex]" 'helm-bibtex)

(defun ans-split-right-if-wide ()
  "Split the window to the right if there is sufficient space."
  (interactive)
  (if (>= (window-total-width) 200)
      (split-window-right -100)
    (split-window-below)))

(defun ans-r-file-here ()
  "Use here::here to determine path for R buffer."
  (shell-command-to-string
   (concat
    "Rscript -e \""
    "setwd(dirname('"(buffer-file-name)"'));"
    "cat(here::here())"
    "\"")))

(defun ans-start-R ()
  "Start R with default options, splitting the window vertically."
  (interactive)
  (ans-split-right-if-wide)
  (save-selected-window
    (other-window 1)
    (R "--no-save --no-restore")))

(defun ans-quit-R ()
  "Quit R process and close buffer."
  (interactive)
  (ess-quit)
  (kill-buffer)
  (delete-window))

(defun ans-switch-to-mru-buffer ()
  "Switch to most-recently-used (MRU) buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun ans--reload-initfile ()
  "Reload the Emacs init file."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(defun ans--edit-initfile ()
  "Edit the Emacs init file."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

;; Emacs server
(use-package edit-server
  :ensure t
  :config
  (edit-server-start))

(provide 'init)
;;; init.el ends here
