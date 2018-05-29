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
	helm-projectile-truncate-lines t)
  :general
  (ans-leader-def
    :states 'normal
    "p" 'helm-projectile
    "P" 'helm-projectile-switch-project)
  (ans-leader-def
    :states '(motion normal)
    :predicate '(ans/in-project-p)
    "b" 'helm-projectile-switch-to-buffer
    "B" 'helm-mini
    "f" 'helm-projectile-find-file
    "F" 'helm-find-files
    "rg" 'helm-projectile-ag)
  :config
  (defvar helm-source-file-not-found
    (helm-build-dummy-source
	"Create file"
      :action 'find-file))
  (add-to-list 'helm-projectile-sources-list helm-source-file-not-found t))

(provide 'init-helm)
;;; init-helm ends here
