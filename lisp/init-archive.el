;; Stuff I don't use anymore

;; Visually-smooth scrolling
(use-package sublimity
  :ensure t
  :disabled
  :diminish
  :init
  (setq sublimity-scroll-weight 10
	sublimity-scroll-drift-length 5)
  :config
  (require 'sublimity-scroll)
  (sublimity-mode 1))

(use-package org-projectile
  :ensure t
  :after (org projectile)
  :config
  (progn
    (org-projectile-per-project)
    (setq org-projectile-per-project-filepath "project-notes.org")
    (setq org-agenda-files (append org-agenda-files (-filter 'file-exists-p (org-projectile-todo-files))))
    (push (org-projectile-project-todo-entry) org-capture-templates))
  (ans-leader-def
    :states '(motion normal emacs)
    "T" 'org-projectile-project-todo-completing-read))

;; Automatically grouped agendas
(use-package org-super-agenda
  :ensure t
  :after org
  :init
  (setq )
  :config
  (org-super-agenda-mode))

;; Slack configuration
(require 'init-slack)

(use-package slack
  :ensure t
  :commands (slack-start)
  :config
  (ans/slack-setup)
  (general-def
    :keymaps 'slack-message-buffer-mode-map
    :states 'normal
    "gk" 'slack-buffer-goto-prev-message
    "gj" 'slack-buffer-goto-next-message)
  (ans-leader-def
    :keymaps 'slack-message-buffer-mode-map
    :states 'normal
    "mm" 'slack-message-write-another-buffer
    "sd" 'slack-message-delete
    "se" 'slack-message-edit
    "sra" 'slack-message-add-reaction
    "srd" 'slack-message-remove-reaction
    "sf" 'slack-file-upload
    "st" 'slack-thread-start
    "ic" 'slack-channel-select
    "im" 'slack-im-select)
  (add-hook 'slack-message-buffer-mode-hook 'ans/slack-message-mode-config)
  (add-hook 'slack-thread-message-buffer-mode-hook 'ans/slack-message-mode-config))

(general-def
  :states '(motion normal)
  :prefix "C-c C-s"
  "c" 'slack-channel-select
  "m" 'slack-im-select
  "t" 'slack-thread-select
  "u" 'slack-select-unread-rooms
  "r" 'slack-select-rooms)


(defun ans/slack-message-mode-config ()
  "Custom configuration for slack message buffer mode."
  (visual-line-mode))

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'libnotify))

(use-package persp-mode
  :ensure t
  :init
  (setq persp-auto-save-opt 0
	persp-auto-resume-time 0)
  (setq persp-keymap-prefix (kbd "<C-SPC>"))
  :config
  (persp-mode 1))

