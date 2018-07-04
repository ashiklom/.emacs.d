;;; init.el --- Alexey Shiklomanov's Emacs init file
;;;
;;; Commentary:
;;;
;;; Code:
;; Use Emacs's internal package manager
(package-initialize)
(require 'package)
(setq package-enable-at-startup nil)

;; Load some common package repositories. MELPA is the big one.
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; Custom file. Mostly, I avoid using custom in favor of ~setq~.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Also load stuff from emacswiki
(add-to-list 'load-path (expand-file-name "emacswiki" user-emacs-directory))

;; ...and from my own custom set of functions
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; For installing packages from source (e.g. GitHub)
(use-package quelpa
  :ensure t
  :init
  (setq quelpa-update-melpa-p nil))
(use-package quelpa-use-package :ensure t)

;; Global settings
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      show-paren-delay 0
      abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory)
      save-abbrevs 'silent
      scroll-margin 2
      scroll-step 1)
(show-paren-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(electric-pair-mode 1)		; auto-close braces, parentheses, etc.
(blink-cursor-mode -1)

(defvar backup-dir (expand-file-name "backups" user-emacs-directory))
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)
(setq auto-save-default nil)

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

;; Aesthetics
(use-package color-theme-sanityinc-tomorrow
  :ensure t)
(color-theme-sanityinc-tomorrow-night)
(set-face-attribute 'default nil :font "Input Mono Narrow-12")

(require 'init-evil)
(require 'init-utils)
(require 'init-helm)
(require 'init-company)
(require 'init-org)
(require 'init-yasnippet)
(require 'init-ess)
(require 'init-latex)

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

;; Swap buffer positions
(use-package buffer-move :ensure t)

(use-package magit
  :ensure t
  :general
  (ans-leader-def
    :states 'normal
    "g s" 'magit-status)
  )

(use-package evil-magit
  :ensure t
  :after magit)

(use-package magithub
  :ensure t
  :after magit
  :config
  (magithub-feature-autoinject t)
  (ans-leader-def
    :states '(normal motion emacs)
    "gd" 'magithub-dashboard)
  (general-def
    :keymaps 'magithub-dash-map
    :states 'normal
    "gu" 'magithub-dashboard-show-read-notifications-toggle))

(use-package diminish :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode) . rainbow-delimiters-mode))

(use-package flycheck
  :ensure t
  :diminish
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

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.Rmd\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc")
  :config
  ;; From aaronbieber/fence-edit.el
  (require 'fence-edit)
  (add-to-list 'fence-edit-blocks '("^```{r.*}" "^```$" R))
  (add-to-list 'fence-edit-blocks '("^```{tikz.*}" "^```$" latex))
  (general-def
    :keymaps 'markdown-mode-map
    :states '(motion normal visual)
    "\\e" 'fence-edit-code-at-point)
  (general-def
    :keymaps 'fence-edit-mode-map
    "C-c C-c" 'fence-edit-exit
    "C-c C-k" 'fence-edit-abort)
  )

;; (use-package mmm-mode
;;   :ensure t
;;   :init
;;   (setq mmm-global-mode 'maybe)
;;   (setq mmm-submode-decoration-level 2)
;;   (setq mmm-parse-when-idle nil)
;;   (setq mmm-idle-timer-delay 0.2)
;;   :config
;;   (mmm-add-classes
;;    '((ans-rmarkdown
;;       :submode r-mode
;;       :front "^```{r.*}[\r\n]"
;;       :back "^```$"
;;       )
;;      (ans-latex
;;       :submode latex-mode
;;       :front "^```{tikz.*}[\r\n]"
;;       :back "^```$")))
;;   (mmm-add-mode-ext-class 'markdown-mode "\\.Rmd\\'" 'ans-rmarkdown)
;;   (mmm-add-mode-ext-class 'markdown-mode "\\.Rmd\\'" 'ans-latex)
;;   )

;; ;; Alternative: Polymode
;; ;; Currently feels buggy
;; (use-package polymode
;;   :ensure t
;;   :mode
;;   (("\\.Rnw" . poly-noweb+r-mode)
;;    ("\\.Rmd" . poly-markdown+r-mode)
;;    ("\\.md" . poly-markdown-mode)))

(use-package simpleclip
  :ensure t
  :config
  (simpleclip-mode 1))

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  :general
  (general-def
    :states 'insert
    "M-e" 'sp-forward-slurp-sexp
    "M-w" 'sp-forward-barf-sexp))

;; For editing text fields in the browser
(use-package edit-server
  :ensure t
  :config
  (edit-server-start))

(use-package adaptive-wrap
  :ensure t
  :diminish
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))

;; (use-package persp-mode
;;   :ensure t
;;   :init
;;   (setq persp-auto-save-opt 0
;; 	persp-auto-resume-time 0)
;;   (setq persp-keymap-prefix (kbd "<C-SPC>"))
;;   :config
;;   (persp-mode 1))

(use-package perspective
  :ensure t
  :commands persp-switch
  :init
  (setq persp-mode-prefix-key (kbd "<C-SPC>"))
  :config
  (persp-mode)
  :general
  (ans-leader-def
    :states '(motion normal visual)
    "o" 'persp-switch
    "[" 'persp-prev
    "]" 'persp-next))

(use-package expand-region
  :ensure t
  :init
  (setq expand-region-contract-fast-key ",")
  :general
  (general-def
    :states 'motion
    "z." 'er/expand-region))

(use-package fill-function-arguments
  :quelpa (fill-function-arguments
	   :fetcher github
	   :repo "davidshepherd7/fill-function-arguments")
  :general
  (general-def
    :states 'normal
    "gs" 'fill-function-arguments-dwim))

(use-package aggressive-indent
  :ensure t
  :hook (prog-mode . aggressive-indent-mode)
  :config
  (ans-leader-def
    :states 'normal
    "I" 'aggressive-indent-mode))

(use-package pdf-tools
  :ensure t
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (pdf-tools-install))

(use-package undo-tree
  :ensure t
  :diminish
  :init
  (setq undo-tree-history-directory-alist `(("." . "~/.emacs.d/undo"))
  	undo-tree-auto-save-history t)
  :config
  (global-undo-tree-mode))

;; Remember previous window configuration
(winner-mode)
(diminish winner-mode)

(provide 'init)
;;; init.el ends here
