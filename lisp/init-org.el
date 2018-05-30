;;; init-org -- My org mode configuration files
;;;
;;; Commentary:
;;;
;;; Code:
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :commands (org-mode org-agenda)
  :init
  (setq org-todo-keywords
	'((sequence "TODO" "STARTED" "|" "DONE")))
  (setq org-capture-templates
	'(("E" "Emacs config" entry
	   (file+headline "~/Dropbox/Notes/emacs.org" "Configuration to-do list")
	   "** TODO %?")
	  ("e" "Emacs note" entry
	   (file+headline "~/Dropbox/Notes/emacs.org" "Misc")
	   "** %?")
	  ("l" "Personal to-do item" entry
	   (file+headline "~/Dropbox/Notes/life.org" "Tasks")
	   "** TODO %?")))
  (setq org-agenda-files '("~/Dropbox/Notes/" "~/Dropbox/Notes/journal/")
	org-agenda-file-regexp "\\`[^.].*\\.org\\'\\|\\`[0-9]+-[0-9]+-[0-9]+\\'")
  (setq org-hide-emphasis-markers t)
  (add-hook 'org-mode-hook (lambda () (linum-mode -1)))
  :general
  (general-def
    :states '(motion normal)
    :keymaps 'org-mode-map
    "<backspace>" 'outline-hide-subtree
    "g t" 'org-todo
    "C-c C-q" 'air-org-set-tags)
  (ans-leader-def
    :states '(motion normal emacs)
    :keymaps 'org-mode-map
    "#" 'org-update-statistics-cookies
    "t" 'air-org-set-tags)
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
  (add-hook 'evil-org-mode-hook 'ans/evil-org-mode-setup)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (general-def
    :keymaps 'evil-org-mode-map
    :states '(motion normal visual)
    "gj" 'outline-next-heading
    "gk" 'outline-previous-heading
    "g$" 'evil-end-of-line
    "g%" 'ans/org-realign-tags))

(defun ans/evil-org-mode-setup ()
  "Custom setup for org mode."
  (push '(?* . ("*" . "*")) evil-surround-pairs-alist)
  (push '(?/ . ("/" . "/")) evil-surround-pairs-alist)
  (evil-org-set-key-theme '(navigation insert textobjects calendar)))

(defun ans/org-realign-tags ()
  "Right-align org mode tags in current buffer."
  (interactive)
  (org-set-tags nil t))

(defun air--org-swap-tags (tags)
  "Replace any tags on the current headline with TAGS.

The assumption is that TAGS will be a string conforming to Org Mode's
tag format specifications, or nil to remove all tags."
  (let ((old-tags (org-get-tags-string))
        (tags (if tags
                  (concat " " tags)
                "")))
    (save-excursion
      (beginning-of-line)
      (re-search-forward
       (concat "[ \t]*" (regexp-quote old-tags) "[ \t]*$")
       (line-end-position) t)
      (replace-match tags)
      (org-set-tags t))))

(defun air-org-set-tags (tag)
  "Add TAG if it is not in the list of tags, remove it otherwise.

TAG is chosen interactively from the global tags completion table."
  (interactive
   (list (let ((org-last-tags-completion-table
                (if (derived-mode-p 'org-mode)
                    (org-uniquify
                     (delq nil (append (org-get-buffer-tags)
                                       (org-global-tags-completion-table))))
                  (org-global-tags-completion-table))))
           (org-icompleting-read
            "Tag: " 'org-tags-completion-function nil nil nil
            'org-tags-history))))
  (let* ((cur-list (org-get-tags))
         (new-tags (mapconcat 'identity
                              (if (member tag cur-list)
                                  (delete tag cur-list)
                                (append cur-list (list tag)))
                              ":"))
         (new (if (> (length new-tags) 1) (concat " :" new-tags ":")
                nil)))
    (air--org-swap-tags new)))

(use-package org-journal
  :ensure t
  :init
  (setq org-journal-dir "~/Dropbox/Notes/journal"
	org-journal-file-format "%Y-%m-%d"))
(evil-ex-define-cmd "now" 'org-journal-new-entry)

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

(defun ans/clean-org-agenda-files ()
  "Remove org agenda files that don't exist."
  (interactive)
  (setq org-agenda-files (-filter 'file-exists-p (org-agenda-files))))

;; Run it once for good measure
(ans/clean-org-agenda-files)

(provide 'init-org)
;;; init-org ends here
