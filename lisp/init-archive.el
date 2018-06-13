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
