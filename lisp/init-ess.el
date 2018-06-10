;;; init-ess -- My ESS configuration
;;;
;;; Commentary:
;;; Emacs Speaks Statistics (ESS)
;;; IDE for R and other stats programs.
;;; Code:
(use-package ess
  :ensure t
  :mode ("\\.[rR]\\'" . r-mode)
  :init
  (add-to-list 'evil-emacs-state-modes 'inferior-ess-mode)
  (add-to-list 'evil-emacs-state-modes 'ess-rdired-mode)
  (setq comint-move-point-for-output t)	; Scroll R buffer on output
  (setq ess-ask-for-ess-directory nil
	ess-directory-function 'ans-r-file-here
	ess-default-style 'RStudio
	ess-use-company t
	ess-eval-visibly 'nowait)
  :config
  (require 'ess-rutils)
  (add-to-list 'ess-R-font-lock-keywords
	       '(ess-fl-keyword:fun-calls . t) t)
  (ans/add-evil-maps 'ess-help-mode-map)
  :general
  (general-def
    :keymaps 'ess-mode-map
    :states 'normal
    :prefix "\\"
    "r f" 'ans-start-R
    "r q" 'ans-quit-R
    "l" 'ess-eval-line
    "d" 'ess-eval-line-and-step
    "f f" 'ess-eval-function
    "p p" 'ess-eval-paragraph
    "p d" 'ess-eval-paragraph-and-step
    "a a" 'ess-eval-buffer
    "a d" 'ess-eval-buffer-from-here-to-end
    "a s" 'ess-eval-buffer-from-beg-to-here
    "r h" 'ess-display-help-on-object
    "v i" 'ess-r-devtools-install-package
    "v d" 'ess-r-devtools-document-package
    "v l" 'ess-r-devtools-load-package
    "r o" 'ess-rutils-objs
    "r p" 'ans/ess-eval-symbol)
  (general-def
    :states 'visual
    :keymaps 'ess-mode-map
    :prefix "\\"
    "s s" 'ess-eval-region)
  (general-def
    :states 'insert
    :keymaps 'ess-mode-map
    "_" 'self-insert-command
    "M-m" (lambda() (interactive)(insert " %>%"))
    "M--" 'ess-insert-S-assign
    "C-c" (lambda() (interactive)(insert "#'")))
  (general-def
    :keymaps 'ess-help-mode-map
    :states 'emacs
    "SPC" 'ans-leader-command)
  (general-def
    :keymaps 'ess-help-mode-map
    :states 'emacs
    :prefix "\\"
    "r h" 'ess-display-help-on-object)
  (general-def
    :keymaps 'ess-rdired-mode-map
    "j" 'ess-rdired-next-line
    "k" 'ess-rdired-previous-line))

(defun ans/inferior-ess-mode-setup ()
  "My custom configuration for inferior-ess-mode."
  (setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions)))

(add-hook 'inferior-ess-mode-hook 'ans/inferior-ess-mode-setup)

(defun ans-split-right-if-wide ()
  "Split the window to the right if there is sufficient space."
  (interactive)
  (if (>= (window-total-width) 200)
      (split-window-right -100)
    (split-window-below)))

(defun ans-r-file-here ()
  "Use here::here to determine path for R buffer."
  (shell-command-to-string
   (concat
    "Rscript -e \""
    "my_dir <- dirname('"(buffer-file-name)"');"
    "t <- tryCatch(setwd(my_dir), error = function(e) NULL);"
    "cat(here::here())"
    "\"")))

(defun ans-start-R ()
  "Start R with default options, splitting the window vertically."
  (interactive)
  (ans-split-right-if-wide)
  (save-selected-window
    (other-window 1)
    (R "--no-save --no-restore")))

(defun ans-quit-R ()
  "Quit R process and close buffer."
  (interactive)
  (ess-quit)
  (kill-buffer)
  (delete-window))

(defun ans/ess-eval-symbol ()
  "Evaluate (usually print) the symbol at point."
  (interactive)
  (save-excursion
    (er/mark-symbol)
    (ess-eval-region (point) (mark) nil)
    (deactivate-mark)))

;; ox-ravel -- Better R integration into org mode
(require 'ox-ravel)

(provide 'init-ess)
;;; init-ess ends here
