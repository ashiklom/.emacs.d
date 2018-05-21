;; Use Emacs's internal package manager
(require 'package)

;; Load some common package repositories. MELPA is the big one.
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-collection magit general evil-surround ess evil-visual-mark-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Additional package manager
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; Global settings
(show-paren-mode 1)
(setq show-paren-delay 0)
(global-linum-mode t)

;; Set the current theme
(load-theme 'wombat)

;; Turn on evil mode by default
(use-package evil
  :ensure t ;; Install if not installed
  :init
  (setq evil-want-integration nil)
  :config   ;; Subsequent commands are for configuration after loading
  (evil-mode 1)
  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1))
  (use-package evil-collection
    :ensure t
    :config
    (evil-collection-init))
  )

;; General package for better key-bindings
(use-package general
  :ensure t)

;; Unbind some common keys
(general-unbind 'motion
  "SPC"
  "C-u"
  "\\")

;; Some custom vim keys
(general-define-key
 :states '(motion normal)
 "S" 'save-buffer)

(defun ans-switch-to-mru-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(general-define-key
 :states '(motion normal visual)
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
 "<left>" 'evil-prev-buffer)

;; Leader and local leader
(general-create-definer ans-leader-def :prefix "SPC")

;; Global leader prefix mappings
(ans-leader-def
 :states 'motion
;; Control-T-like behavior
 "t" 'find-file
 "b" 'switch-to-buffer)

;; Remap <jk> to escape in insert mode
(general-define-key
 :states '(insert)
 "j"
 (general-key-dispatch 'self-insert-command
   :timeout 0.25
   "k" 'evil-normal-state))

;; Reload emacs init file with "<leader>sv"
(defun ans--reload-initfile ()
  "Reload the emacs init file"
  (interactive)
  (load-file "~/.emacs.d/init.el"))
(defun ans--edit-initfile ()
  "Edit the emacs init file"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(ans-leader-def
  :states '(motion normal)
  "s v" 'ans--reload-initfile
  "e v" 'ans--edit-initfile
  "s x" (lambda() (interactive)(switch-to-buffer "*scratch*")))

(use-package magit
  :ensure t
  :general
  (ans-leader-def
    :states 'motion
    "g s" 'magit-status))

;; Org mode
(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "VERIFY" "|" "DONE")))

;; Common org-mode notes
(ans-leader-def
  :states 'motion
  "o e" (lambda () (interactive)(find-file "~/Dropbox/Notes/emacs.org"))) 

;; Org mode mappings
(general-define-key
 :states '(motion normal)
 :keymaps 'org-mode-map
 "RET" 'org-cycle
 "<backspace>" 'outline-hide-subtree
 "<C-return>" 'org-open-at-point
 "g t" 'org-todo
 "g o" (lambda() (interactive)(evil-end-of-line)(org-insert-heading-respect-content)(evil-append 1))
 "g O" (lambda() (interactive)(evil-beginning-of-line)(org-insert-heading-respect-content)(evil-append 1)))

;; Lisp interaction mode
; (evil-define-key 'insert global-map (kbd "<C-return>") 'eval-last-sexp)
(general-define-key
 :states '(motion normal)
 :keymaps 'lisp-mode-shared-map
 "<C-return>" 'eval-defun)

(use-package ess
  :ensure t
  :init
  (add-to-list 'evil-emacs-state-modes 'inferior-ess-mode)
  :custom
  (ess-ask-for-ess-directory nil)
  (ess-directory-function 'ans-r-file-here)
  (ess-default-style 'RStudio)
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
	   "M--" 'ess-insert-S-assign)
  (general-define-key
   :keymaps 'evil-emacs-state-map
   "C-S-j" 'evil-window-down
   "C-S-k" 'evil-window-up
   "C-S-h" 'evil-window-left
   "C-S-l" 'evil-window-right)
  )

;; ESS mode mappings
(defun ans-split-right-if-wide ()
  (interactive)
  (if (>= (window-total-width) 200)
      (split-window-right -100)
    (split-window-below)))

;; Use here::here to determine path for R buffer
(defun ans-r-file-here ()
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
