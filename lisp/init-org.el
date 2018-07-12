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
	'((sequence "TODO" "NEXT" "|" "DONE" "CANCELED")))
  (setq org-capture-templates
	'(("E" "Emacs config" entry
	   (file+headline "~/Dropbox/Notes/emacs.org" "Configuration to-do list")
	   "** TODO %?")
	  ("e" "Emacs note" entry
	   (file+headline "~/Dropbox/Notes/emacs.org" "Misc")
	   "** %?")
	  ("t" "TODO" entry
	   (file "~/Dropbox/Notes/unsorted.org")
	   "* TODO %?\n%U\n%a\n") ; :clock-in t :clock-resume t
	  ("u" "Miscellaneous note" entry
	   (file "~/Dropbox/Notes/unsorted.org")
	   "* %? :NOTE:\n%U\n%a\n")))
  (setq org-hide-emphasis-markers nil)	; Toggle with ans/org-toggle-emphasis-markers
  (setq org-babel-load-languages '((emacs-lisp . t) (R . t))
	org-src-fontify-natively t)
  (setq org-agenda-files '("~/Dropbox/Notes/")
	org-agenda-custom-commands
	'((" " "Agenda"
	   ((agenda "" nil)
	    (tags "REFILE"
		  ((org-agenda-overriding-header "Tasks to Refile")
		   (org-tags-match-list-sublevels nil)))
	    (tags-todo "-REFILE-emacs"
		       ((org-agenda-overriding-header "Other tasks")))
	    (tags-todo "emacs"
		       ((org-agenda-overriding-header "Emacs configuration")
			(org-agenda-sorting-strategy '(todo-state-down)))))
	   nil)))
  (setq org-refile-targets '((nil :maxlevel . 9)
			     (org-agenda-files :maxlevel . 9))
	org-refile-use-outline-path 'file
	org-outline-path-complete-in-steps nil
	org-refile-allow-creating-parent-nodes 'confirm
	org-refile-target-verify-function 'ans/verify-refile-target)
  (add-hook 'org-mode-hook (lambda () (linum-mode -1)))
  :general
  (general-def
    :keymaps 'org-mode-map
    "C-c C-q" 'air-org-set-tags)
  (general-def
    :states '(motion normal)
    :keymaps 'org-mode-map
    "<backspace>" 'outline-hide-subtree
    "gt" 'org-todo
    "gj" 'outline-next-heading
    "gk" 'outline-previous-heading
    "g$" 'evil-end-of-line
    "g%" 'ans/org-realign-tags
    "go" 'ans/evil-insert-heading-after-current
    "gO" 'ans/evil-insert-heading)
  (general-def
    :states 'visual
    :keymaps 'org-mode-map
    :prefix "\\"
    "ss" 'eval-region)
  (ans-leader-def
    :keymaps 'org-mode-map
    :states '(motion normal emacs)
    "#" 'org-update-statistics-cookies
    "%" 'ans/org-toggle-emphasis-markers)
  (ans-leader-def
    :keymaps 'org-mode-map
    :states '(motion normal visual)
    "L" 'org-insert-last-stored-link
    "ss" 'org-schedule
    "sd" 'org-deadline)
  (general-def
    :states '(motion)
    :keymaps 'calendar-mode-map
    "h" 'calendar-backward-day
    "l" 'calendar-forward-day
    "k" 'calendar-backward-week
    "j" 'calendar-forward-week
    "H" 'calendar-backward-month
    "L" 'calendar-forward-month))

(defun ans/evil-insert-heading ()
  "Insert heading before point and enter insert mode."
  (interactive)
  (org-insert-heading)
  (evil-insert 1))

(defun ans/evil-insert-heading-after-current ()
  "Insert heading after point and enter insert mode."
  (interactive)
  (org-insert-heading-respect-content)
  (evil-insert 1))

(defun ans/verify-refile-target ()
  "Exclude TODO keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook 'ans/evil-org-mode-setup)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

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
           (completing-read
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
	org-journal-file-format "%Y-%m-%d"
	org-journal-enable-agenda-integration t))
(evil-ex-define-cmd "now" 'org-journal-new-entry)

(defun ans/clean-org-agenda-files ()
  "Remove org agenda files that don't exist."
  (interactive)
  (setq org-agenda-files (-filter 'file-exists-p (org-agenda-files))))

;; Run it once for good measure
(ans/clean-org-agenda-files)

;; Custom source listing all agenda files
(defun ans/helm-org-agenda-list-files ()
  "Helm source listing all current org agenda files."
  (interactive)
  (helm :sources (helm-build-sync-source
		     "Org agenda files"
		   :candidates (org-agenda-files)
		   :action '(("Open file" . find-file)))
	:buffer "*helm agenda files*"))

(defun ans/org-toggle-emphasis-markers ()
  "Toggle the display of org emphasis markers."
  (interactive)
  (if org-hide-emphasis-markers
      (setq org-hide-emphasis-markers nil)
    (setq org-hide-emphasis-markers t))
  (font-lock-flush))

(use-package org-capture-pop-frame
  :ensure t)

(use-package toc-org
  :ensure t
  :config
  (add-hook 'org-mode-hook 'toc-org-enable))

(provide 'init-org)
;;; init-org ends here
