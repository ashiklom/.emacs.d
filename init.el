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
      show-paren-delay 0)
(show-paren-mode 1)
(tool-bar-mode -1)
(electric-pair-mode 1)		; Auto-close braces, parentheses, etc.
(global-linum-mode t)		; Enable line numbers by default

;; Set the current theme
(load-theme 'wombat)

;; General package for better key-bindings
(use-package general
  :ensure t
  :demand
  :config
  (general-create-definer ans-leader-def :prefix "SPC"))

;; Turn on evil mode by default
(use-package evil
  :ensure t
  :demand
  :init
  (setq evil-want-integration nil)
  :config
  (evil-mode 1)
  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1))
  :general
  (general-unbind 'motion
    "SPC"
    "C-u"
    "\\")
  (:states 'normal "S" 'save-buffer)
  (:states 'motion
	   ;; Quickly switch between windows
	   "C-S-j" 'evil-window-down
	   "C-S-k" 'evil-window-up
	   "C-S-h" 'evil-window-left
	   "C-S-l" 'evil-window-right
	   ;; Scroll up (default vim behavior, but overridden by Emacs)
	   "C-u" 'evil-scroll-up
	   ;; More convenient buffer switching
	   "TAB" 'ans-switch-to-mru-buffer
	   "<right>" 'evil-next-buffer
	   "<left>" 'evil-prev-buffer
	   "Q" 'kill-this-buffer)
  (:keymaps 'evil-emacs-state-map
	   "C-S-j" 'evil-window-down
	   "C-S-k" 'evil-window-up
	   "C-S-h" 'evil-window-left
	   "C-S-l" 'evil-window-right)
  (ans-leader-def
    :states 'motion
    "t" 'helm-find-files
    "b" 'switch-to-buffer
    "e e" 'dired
    "s v" 'ans--reload-initfile
    "e v" 'ans--edit-initfile
    "s x" (lambda() (interactive)(switch-to-buffer "*scratch*")))
  (general-def
    :states '(insert)
    "j"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "k" 'evil-normal-state))
  (:states '(motion normal)
	   :keymaps 'lisp-mode-shared-map
	   "<C-return>" 'eval-defun)
  )
;; Evil-collection was causing problems with Helm.
;; This is basic code for setting it up -- I might get back to it later.
  ; (use-package evil-collection
  ;   :ensure t
  ;   :init
  ;   :config
  ;   (evil-collection-init))

;; Swap buffer positions
(use-package buffer-move
  :ensure t
  :general
  (:states 'motion
	   "<C-S-up>" 'buf-move-up
	   "<C-S-down>" 'buf-move-down
	   "<C-S-left>" 'buf-move-left
	   "<C-S-right>" 'buf-move-right))

(use-package magit
  :ensure t
  :general
  (ans-leader-def
    :states 'motion
    "g s" 'magit-status)
  :config
  (use-package evil-magit
    :ensure t)
  )

(use-package git-gutter-fringe
  :ensure t
  :demand
  :init
  (setq-default left-fringe-width 20)
  (setq git-gutter:ask-p nil)
  :config
  (global-git-gutter-mode +1)
  ;; (git-gutter:linum-setup)
  :general
  (:states 'normal
	   "] c" 'git-gutter:next-hunk
	   "[ c" 'git-gutter:previous-hunk)
  (ans-leader-def
    :states 'normal
    "h s" 'git-gutter:stage-hunk
    "h u" 'git-gutter:revert-hunk
    "h d" 'git-gutter:popup-hunk)
  )

(use-package helm
  :ensure t
  :demand
  :init
  (require 'helm-config)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 20)
  (setq helm-mode-fuzzy-match t)
  :config
  (helm-mode 1)
  :general
  (:states 'motion
	   "M-x" 'helm-M-x))

(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (use-package helm-projectile
    :ensure t
    :config
    (helm-projectile-on)
    :general
    (ans-leader-def
      :states 'motion
      "f" 'helm-projectile-find-file
      "p" 'helm-projectile-switch-project)))

(use-package evil-nerd-commenter
  :ensure t
  :general
  (ans-leader-def
    :states '(normal visual)
    "c c" 'evilnc-comment-or-uncomment-lines
    "c p" 'evilnc-comment-or-uncomment-paragraphs))

(use-package fuzzy
  :ensure t)

(use-package company
  :ensure t
  :demand
  :init
  (setq company-selection-wrap-around t)
  (setq company-idle-delay 0.5)
  :config
  (global-company-mode)
  :general
  (:keymaps 'company-active-map
	    "ESC" 'company-abort
	    "TAB" 'company-complete-common-or-cycle
	    "C-n" 'company-select-next
	    "C-p" 'company-select-previous))

(use-package org
  :ensure t
  :init
  (setq org-todo-keywords
	'((sequence "TODO" "STARTED" "VERIFY" "|" "DONE")))
  :general
  (ans-leader-def
    :states 'motion
    "o e" (lambda () (interactive)(find-file "~/Dropbox/Notes/emacs.org"))) 
  (general-def
    :states '(motion normal)
    :keymaps 'org-mode-map
    "RET" 'org-cycle
    "<backspace>" 'outline-hide-subtree
    "<C-return>" 'org-open-at-point
    "g t" 'org-todo
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
  (:states 'motion
	   :keymaps 'ess-mode-map
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
  (general-define-key
   :keymaps 'evil-emacs-state-map
   "C-S-j" 'evil-window-down
   "C-S-k" 'evil-window-up
   "C-S-h" 'evil-window-left
   "C-S-l" 'evil-window-right)
  )

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
