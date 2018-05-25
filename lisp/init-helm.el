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
  :config
  (defvar helm-source-file-not-found
    (helm-build-dummy-source
	"Create file"
      :action 'find-file))
  (add-to-list 'helm-projectile-sources-list helm-source-file-not-found t))

(provide 'init-helm)
;;; init-helm ends here
