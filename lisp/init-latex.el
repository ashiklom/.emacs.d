;;; init-latex -- My configuration for LaTeX
;;;
;;; Commentary:
;;;
;;; Code:
(use-package tex
  :defer t
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'ans-latex-mode-setup)
  :config
  (use-package auctex-latexmk
    :ensure t
    :init
    (setq auctex-latexmk-inherit-TeX-PDF-mode t)
    :config
    (auctex-latexmk-setup))
  )

(use-package helm-bibtex
  :ensure t
  :commands helm-bibtex)
(evil-ex-define-cmd "bib[tex]" 'helm-bibtex)

(defun ans-latex-mode-setup ()
  "Set custom options for LaTeX files."
  (require 'reftex)
  ;; Use settings for text mode
  (ans-text-mode-setup)
  ;; Use the "default" vim paragraph definition
  (setq paragraph-start "\f\\|[ 	]*$")
  (setq paragraph-separate "[ 	\f]*$")
  )

(use-package evil-latex-textobjects
  :quelpa (evil-latex-textobjects :fetcher github :repo "hpdeifel/evil-latex-textobjects")
  :hook (LaTeX-mode-hook . turn-on-evil-latex-textobjects-mode))

(provide 'init-latex)
;;; init-latex ends here
