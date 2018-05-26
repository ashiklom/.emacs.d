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
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

;; Custom file. Mostly, I avoid using custom in favor of ~setq~.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Also load stuff from emacswiki
(add-to-list 'load-path (expand-file-name "emacswiki" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

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

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode) . rainbow-delimiters-mode))

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

;; Emacs server
(use-package edit-server
  :ensure t
  :config
  (edit-server-start))

(provide 'init)
;;; init.el ends here
