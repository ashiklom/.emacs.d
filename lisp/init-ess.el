;;; init-ess -- My ESS configuration
;;;
;;; Commentary:
;;; Emacs Speaks Statistics (ESS)
;;; IDE for R and other stats programs.
;;; Code:
(use-package ess
  :ensure t
  :mode ("\\.[RR]\\'" . ess-r-mode)
  :init
  (add-to-list 'evil-emacs-state-modes 'inferior-ess-mode)
  (setq comint-move-point-for-output t)	; Scroll R buffer on output
  (setq ess-ask-for-ess-directory nil
	ess-directory-function 'ans-r-file-here
	ess-default-style 'RStudio
	ess-use-company t)
  :config
  (add-to-list 'ess-R-font-lock-keywords
	       '(ess-fl-keyword:fun-calls . t) t)
  :general
  (:keymaps 'ess-mode-map
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
	    "v l" 'ess-r-devtools-load-package)
  (:states 'visual
	   :keymaps 'ess-mode-map
	   :prefix "\\"
	   "s s" 'ess-eval-region)
  (:states 'insert
	   :keymaps 'ess-mode-map
	   "M-m" (lambda() (interactive)(insert " %>%"))
	   "M--" 'ess-insert-S-assign
	   "C-c" (lambda() (interactive)(insert "#'")))
  )

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
    "setwd(dirname('"(buffer-file-name)"'));"
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


(provide 'init-ess)
;;; init-ess ends here
