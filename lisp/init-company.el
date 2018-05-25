;;; init-company -- Configuration for Company
;;;
;;; Commentary:
;;;
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
    "C-f" 'ans-company-current-directory-backend
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

(defun ans-company-current-directory-backend (command &optional arg &rest ignored)
  "Complete files in the current directory.
COMMAND company completion command.
ARG company completion arguments.
IGNORED company ignored arguments."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'ans-company-current-directory-backend))
    (prefix (company-grab-symbol))
    (candidates
     (remove-if-not
      (lambda (c) (string-prefix-p arg c))
      (directory-files ".")))
    ))

(provide 'init-company)
;;; init-company ends here
