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
  (setq org-hide-emphasis-markers t)
  (add-hook 'org-mode-hook (lambda () (linum-mode -1)))
  :general
  (general-def
    :states '(motion normal)
    :keymaps 'org-mode-map
    "<backspace>" 'outline-hide-subtree
    "g t" 'org-todo)
  (ans-leader-def
    :states '(motion normal emacs)
    :keymaps 'org-mode-map
    "#" 'org-update-statistics-cookies)
  (general-def
    :states '(motion)
    :keymaps 'calendar-mode-map
    "h" 'calendar-backward-day
    "l" 'calendar-forward-day
    "k" 'calendar-backward-week
    "j" 'calendar-forward-week
    "H" 'calendar-backward-month
    "L" 'calendar-forward-month))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook (lambda () (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(add-hook 'evil-org-mode-hook
	  (lambda ()
	    (push '(?* . ("*" . "*")) evil-surround-pairs-alist)
	    (push '(?/ . ("/" . "/")) evil-surround-pairs-alist)))

(use-package org-journal
  :ensure t
  :init
  (setq org-journal-dir "~/Dropbox/Notes/journal"
	org-journal-file-format "%Y-%m-%d"))
(evil-ex-define-cmd "now" 'org-journal-new-entry)

(provide 'init-org)
;;; init-org ends here
