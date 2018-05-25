;;; init-org -- My org mode configuration files
;;;
;;; Commentary:
;;;
;;; Code:
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :init
  (setq org-todo-keywords
	'((sequence "TODO" "STARTED" "VERIFY" "|" "DONE" "CANCELED")))
  (setq org-capture-templates
	'(("E" "Emacs config"
	   entry (file+headline "~/Dropbox/Notes/emacs.org" "Configuration to-do list")
	   "** TODO %?")
	  ("e" "Emacs note"
	   entry (file+headline "~/Dropbox/Notes/emacs.org" "Misc")
	   "** %?")))
  (add-hook 'org-mode-hook (lambda () (linum-mode -1)))
  :general
  (general-def
    :states '(motion normal)
    :keymaps 'org-mode-map
    "<backspace>" 'outline-hide-subtree)
  (ans-leader-def
    :states '(motion normal emacs)
    "C" 'org-capture
    "#" 'org-update-statistics-cookies
    "g t" 'org-todo))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook (lambda () (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


(provide 'init-org)
;;; init-org ends here
