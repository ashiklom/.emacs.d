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
 '(package-selected-packages (quote (evil-surround ess evil-visual-mark-mode))))
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

;; Turn on evil mode by default
(use-package evil
  :ensure t ;; Install if not installed
  :config   ;; Subsequent commands are for configuration after loading
  (evil-mode))

;; Enable surround plugin
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; General package for better key-bindings

;; Set the current theme
(load-theme 'wombat)

;; Some custom vim keys
(define-key evil-normal-state-map "S" 'save-buffer)

;; Leader key
;; Note that key-chords need to be defined via variables, as shown here
(defvar ans--leader-global (make-sparse-keymap)
  "Keymap for leader key shortcuts")
(define-key evil-motion-state-map "," ans--leader-global)
(define-key ans--leader-global "f" 'find-file)
(define-key ans--leader-global "b" 'switch-to-buffer)

;; Reload emacs init file with ",sv"
;; Note again the use of `defvar` to set up the keymap
;; Note also the use of 'interactive' -- this doesn't work otherwise
(defun ans--reload-initfile ()
  "Reload the emacs init file"
  (interactive)
  (load-file "~/.emacs.d/init.el"))
(defun ans--edit-initfile ()
  "Edit the emacs init file"
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(defvar ans--leader-s (make-sparse-keymap) "Leader-s keymap")
(define-key ans--leader-global "s" ans--leader-s)
(define-key ans--leader-s "v" 'ans--reload-initfile)

(defvar ans--leader-e (make-sparse-keymap) "Leader-e keymap")
(define-key ans--leader-global "e" ans--leader-e)
(define-key ans--leader-e "v" 'ans--edit-initfile)

;; Window motions
;; Note the use of `motion` state map so this works in read-only windows
(define-key evil-motion-state-map (kbd "C-S-j") 'evil-window-down)
(define-key evil-motion-state-map (kbd "C-S-k") 'evil-window-up)
(define-key evil-motion-state-map (kbd "C-S-h") 'evil-window-left)
(define-key evil-motion-state-map (kbd "C-S-l") 'evil-window-right)

;; Filetype associations
(add-to-list 'auto-mode-alist '("\\.R\\'" . ess-r-mode))

;; Org mode
(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "VERIFY" "|" "DONE")))

;; Lisp interaction mode
; (evil-define-key 'insert global-map (kbd "<C-return>") 'eval-last-sexp)
(evil-define-key 'normal lisp-mode-shared-map (kbd "<C-return>") 'eval-defun)

;(evil-define-key 'normal 'org-mode-map
;  (kbd "RET") 'org-cycle)
