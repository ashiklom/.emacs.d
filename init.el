;; Use Emacs's internal package manager
(package-initialize)
(require 'package)
(setq package-enable-at-startup nil)

;; Load some common package repositories. MELPA is the big one.
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

;; Custom file. Mostly, I avoid using custom in favor of ~setq~.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; Global settings
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startub-echo-area-message t
      show-paren-delay 0
      abbrev-file-name "~/.emacs.d/abbrev_defs"
      save-abbrevs 'silent)
(show-paren-mode 1)
(tool-bar-mode -1)
(electric-pair-mode 1)		; Auto-close braces, parentheses, etc.

(defvar backup-dir "~/.emacs.d/backups")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)

;; I want line numbers for programming (prog) and text modes
(defun ans-prog-mode-setup ()
  (linum-mode 1)
  (toggle-truncate-lines 1)
  (flyspell-prog-mode))
(defun ans-text-mode-setup ()
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
  ;; Quickly switch between windows
  "C-S-j" 'evil-window-down
  "C-S-k" 'evil-window-up
  "C-S-h" 'evil-window-left
  "C-S-l" 'evil-window-right
  "C-=" 'evil-window-increase-height
  "C--" 'evil-window-decrease-height
  "C-+" 'evil-window-increase-width
  "C-_" 'evil-window-decrease-width
  "C-0" 'balance-windows
  "C-)" 'shrink-window-if-larger-than-buffer
  "<C-M-down>" 'evil-window-increase-height
  "<C-S-up>" 'buf-move-up
  "<C-S-down>" 'buf-move-down
  "<C-S-left>" 'buf-move-left
  "<C-S-right>" 'buf-move-right
  "C-d" 'evil-scroll-down
  "C-u" 'evil-scroll-up
  )

(general-def
  :states 'normal
  "S" 'save-buffer
  ;; More convenient buffer switching
  "TAB" 'ans-switch-to-mru-buffer
  "<S-right>" 'evil-next-buffer
  "<S-left>" 'evil-prev-buffer
  "<S-up>" 'other-frame
  "<S-down>" 'other-frame
  "Q" 'kill-this-buffer
  ;; Helm
  "M-x" 'helm-M-x
  )

(general-create-definer ans-leader-def :prefix "SPC")
(ans-leader-def
  :states 'motion
  "b" 'switch-to-buffer
  "sv" 'ans--reload-initfile
  "ev" 'ans--edit-initfile
  "sx" (lambda() (interactive)(switch-to-buffer "*scratch*"))
  "E" 'new-frame
  "oe" (lambda () (interactive)(find-file "~/Dropbox/Notes/emacs.org"))
  "ps" 'toggle-truncate-lines
  "ss" 'delete-trailing-whitespace
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
    "c c" 'evilnc-comment-or-uncomment-lines
    "c p" 'evilnc-comment-or-uncomment-paragraphs
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
  (defun ans-company-complete-continue ()
    "Insert the result of a completion, then re-start completion.
This makes repeat completions easier (e.g. when completing long file paths).
"
    (interactive)
    (company-complete-selection)
    (company-complete))
  :general
  (general-def
    :states 'insert
    ;; "TAB" 'company-indent-or-complete-common
    ;; See below for discussion of company-dabbrev-code
    ;; https://github.com/company-mode/company-mode/issues/360
    "C-n" 'company-dabbrev-code
    "C-p" 'company-dabbrev-code
    "C-f" 'company-files
    "C-l" 'company-complete
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

(use-package org
  :ensure t
  :init
  (setq org-todo-keywords
	'((sequence "TODO" "STARTED" "VERIFY" "|" "DONE" "CANCELED")))
  (add-hook 'org-mode-hook (lambda () (linum-mode -1)))
  :general
  (general-def
    :keymaps 'org-mode-map
    :states 'normal
    "RET" 'org-cycle
    [tab] 'ans-switch-to-mru-buffer
    "<backspace>" 'outline-hide-subtree
    "<C-return>" 'org-open-at-point
    "g t" 'org-todo
    "g j" 'org-forward-heading-same-level
    "g k" 'org-backward-heading-same-level
    "g h" 'outline-up-heading
    "g o" (lambda() (interactive)(evil-end-of-line)(org-insert-heading-respect-content)(evil-append 1))
    "g O" (lambda() (interactive)(evil-beginning-of-line)(org-insert-heading-respect-content)(evil-append 1)))
  )

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
  :init
  (require 'smartparens-config)
  :hook ((prog-mode) . smartparens-strict-mode)
  :general
  (general-def
    :states 'insert
    "M-e" 'sp-forward-slurp-sexp
    "M-w" 'sp-forward-barf-sexp))

(defun ans-split-right-if-wide ()
  (interactive)
  (if (>= (window-total-width) 200)
      (split-window-right -100)
    (split-window-below)))

(defun ans-r-file-here ()
  "Use here::here to determine path for R buffer"
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
  "Quit R process and close buffer"
  (interactive)
  (ess-quit)
  (kill-buffer)
  (delete-window))

(defun ans-switch-to-mru-buffer ()
  "Switch to most-recently-used (MRU) buffer"
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun ans--reload-initfile ()
  "Reload the emacs init file"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun ans--edit-initfile ()
  "Edit the emacs init file"
  (interactive)
  (find-file "~/.emacs.d/init.el"))
