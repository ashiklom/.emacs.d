;;; init-helm -- Configuration for helm and related utilities
;;;
;;; Commentary:
;;;
;;; Code:
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
  ;; (setq helm-grep-ag-command
  ;; 	"rg --color=always --smart-case --no-heading --line-number %s %s %s")
  (setq helm-autoresize-max-height 40)
  (setq helm-display-function 'ans/helm-hsplit-frame)
  (setq helm-findutils-search-full-path t)
  ;; (setq find-program "fd")
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)
  ;; (use-package helm-rg :ensure t)
  (use-package helm-ag :ensure t)
  :general
  (ans-leader-def
    :states 'normal
    "f" 'helm-find-files
    "F" 'helm-find)
  (general-def
    :keymaps 'helm-map
    "TAB" 'helm-execute-persistent-action
    "<right>" 'right-char
    "<left>" 'left-char
    "C-z" 'helm-select-action
    "C-n" 'helm-next-line
    "C-p" 'helm-previous-line
    "C-S-n" 'helm-next-source
    "C-S-p" 'helm-previous-source)
  )

(defun ans/hsplit-frame ()
  "Split window entirely below the current frame."
  (split-window (frame-root-window) nil 'below))

(defun ans/helm-hsplit-frame (buffer &optional _resume)
  "Open new window below frame, switch to it, and open BUFFER."
  (ans/hsplit-frame)
  (evil-window-bottom-right)
  (switch-to-buffer buffer))

(use-package projectile
  :ensure t
  ;; :requires helm-rg
  :requires helm-ag
  :config
  (projectile-mode))

(defun ans/in-project-p ()
  "Check if current buffer is in a projectile project."
  (ignore-errors (projectile-project-root)))

(use-package helm-projectile
  :ensure t
  :init
  (setq helm-projectile-fuzzy-match t
	helm-projectile-truncate-lines t
	projectile-completion-system 'helm
	projectile-switch-project-action 'helm-projectile)
  :config
  (helm-projectile-on)
  :general
  (ans-leader-def
    :states 'normal
    "p" 'helm-projectile
    "P" 'helm-projectile-switch-project)
  (ans-leader-def
    :states '(motion normal)
    "rg" (general-predicate-dispatch 'helm-ag
	   (ans/in-project-p) 'helm-projectile-ag)))

(use-package helm-org-rifle
  :ensure t
  :general
  (ans-leader-def
    :states 'normal
    "a" 'helm-org-rifle-agenda-files
    "A" 'ans/helm-org-agenda-list-files))

(use-package helm-swoop
  :ensure t
  :init
  (setq helm-swoop-split-direction 'split-window-horizontally)
  :general
  (ans-leader-def
    :states '(motion normal)
    "ii" 'helm-swoop
    "ib" 'helm-multi-swoop-all
    "ip" 'helm-multi-swoop-projectile
    "i0" 'helm-swoop-back-to-last-point))

(use-package helm-descbinds
  :ensure t
  :after helm
  :config
  (helm-descbinds-mode))

(provide 'init-helm)
;;; init-helm ends here
