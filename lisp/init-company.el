;;; init-company -- Configuration for Company
;;; Commentary:
;;; Package-Requires: ((dash "2.14.1"))
;;; Code:

(use-package company
  :ensure t
  :commands global-company-mode
  :diminish 'company-mode
  :init
  (setq company-selection-wrap-around t)
  (setq company-idle-delay nil)
  :config
  (global-company-mode)
  ;; Thanks to this:
  ;; https://github.com/otijhuis/evil-emacs.d/blob/7c122b0e05c367192444a85d12323487422b793b/config/evil-settings.el#L38-L39
  (add-hook 'evil-insert-state-exit-hook (lambda ()(company-abort)))
  ;; See discussion in: https://github.com/expez/company-quickhelp/issues/17
  (add-hook 'company-completion-started-hook 'ans/set-company-maps)
  (add-hook 'company-completion-finished-hook 'ans/unset-company-maps)
  (add-hook 'company-completion-cancelled-hook 'ans/unset-company-maps)
  :general
  (general-def
    :states 'insert
    ;; See below for discussion of company-dabbrev-code
    ;; https://github.com/company-mode/company-mode/issues/360
    "C-f" 'ans/directory-file-backend
    "C-l" 'company-complete		; Note that this includes company-files
    )
  (general-def
    :states 'insert
    :keymaps 'prog-mode-map
    "C-n" 'company-dabbrev-code
    "C-p" 'company-dabbrev-code
    "C-S-n" 'company-dabbrev
    "C-S-p" 'company-dabbrev)
  (general-def
    :states 'insert
    :keymaps 'text-mode-map
    "C-n" 'company-dabbrev
    "C-p" 'company-dabbrev))

(use-package company-quickhelp
  :ensure t
  :diminish 'company-quickhelp-mode
  :after company
  :config
  (company-quickhelp-mode))

(defun ans/unset-company-maps (&rest unused)
  "Set default mappings (outside of company).
Arguments (UNUSED) are ignored."
  (general-def
    :states 'insert
    :keymaps 'override
    "C-n" nil
    "C-p" nil
    "C-l" nil))

(defun ans/set-company-maps (&rest unused)
  "Set maps for when you're inside company completion.
Arguments (UNUSED) are ignored."
  (general-def
    :states 'insert
    :keymaps 'override
    "C-n" 'company-select-next
    "C-p" 'company-select-previous
    "C-l" 'ans-company-complete-continue))

(defun ans-company-complete-continue ()
  "Insert the result of a completion, then re-start completion.
This makes repeat completions easier (e.g. when completing long file paths)."
  (interactive)
  (company-complete-selection)
  (company-complete))

(use-package dash :ensure t)

(defun ans/directory-completion-candidates (prefix)
  "List files in projectile or current buffer directory that match PREFIX."
  (let* ((starting-directory
	  (condition-case nil
	      (projectile-project-root)
	    (error "./")))
	 (my-prefix-base (file-name-nondirectory prefix))
	 (my-prefix-dir (file-name-directory prefix))
	 (my-complete-dir (concat starting-directory my-prefix-dir))
	 (my-completions-all
	  (file-name-all-completions my-prefix-base my-complete-dir))
	 (my-completions (-difference my-completions-all '("./" "../"))))
    (mapcar (lambda (file) (concat my-prefix-dir file)) my-completions)))

(defun ans/directory-file-backend (command &optional arg &rest ignored)
  "Complete files in current or projectile project directory.

COMMAND is command called by company.
ARG is the set of company completion arguments.
IGNORED are arguments ignored by company."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'ans/directory-file-backend))
    (prefix (company-grab-line "\\(?:[\"\']\\|\\s-\\|^\\)\\(.*?\\)" 1))
    (candidates
     (remove-if-not
      (lambda (c) (string-prefix-p arg c))
      (ans/directory-completion-candidates arg)))))

(defun ans/org-keyword-backend (command &optional arg &rest ignored)
  "Completion backend for org keywords (COMMAND, ARG, IGNORED)."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'org-keyword-backend))
    (prefix (and (eq major-mode 'org-mode)
                 (cons (company-grab-line "^#\\+\\(\\w*\\)" 1)
                       t)))
    (candidates (mapcar #'upcase
                        (cl-remove-if-not
                         (lambda (c) (string-prefix-p arg c))
                         (pcomplete-completions))))
    (ignore-case t)
    (duplicates t)))

(add-to-list 'company-backends 'ans/org-keyword-backend)

(provide 'init-company)
;;; init-company ends here
