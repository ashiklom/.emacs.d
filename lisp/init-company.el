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
  :general
  (general-def
    :states 'insert
    ;; "TAB" 'company-indent-or-complete-common
    ;; See below for discussion of company-dabbrev-code
    ;; https://github.com/company-mode/company-mode/issues/360
    "C-n" 'company-dabbrev-code
    "C-p" 'company-dabbrev-code
    "C-f" 'ans/directory-file-backend
    "C-l" 'company-complete		; Note that this includes company-files
    )
  (general-def
    :keymaps 'company-active-map
    "ESC" 'company-abort
    "TAB" 'company-complete-common
    "C-l" 'ans-company-complete-continue
    "SPC" 'company-complete-common-or-cycle
    "C-n" 'company-select-next
    "C-p" 'company-select-previous)
  )

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

(provide 'init-company)
;;; init-company ends here
