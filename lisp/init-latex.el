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
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
	TeX-source-correlate-start-server t)
  :config
  (add-hook 'LaTeX-mode-hook 'ans-latex-mode-setup)
  (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook #'TeX-revert-document-buffer))

(use-package auctex-latexmk
  :ensure t
  :after tex
  :init
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  (auctex-latexmk-setup))

(use-package helm-bibtex
  :ensure t
  :init
  (setq bibtex-completion-bibliography ans/reference-bibfile
	bibtex-completion-library-path ans/reference-dir-pdfs
	bibtex-completion-notes-path ans/reference-notes
	bibtex-autokey-name-case-convert-function 'downcase
	bibtex-autokey-name-year-separator "_"
	bibtex-autokey-year-title-separator "_"
	bibtex-autokey-year-length 4
	bibtex-autokey-titlewords 1
	bibtex-autokey-titleword-length nil
	bibtex-autokey-titleword-case-convert-function 'downcase)
  :commands helm-bibtex)
(evil-ex-define-cmd "bib[tex]" 'helm-bibtex)

(use-package org-ref
  :ensure t
  :init
  (setq org-ref-bibliography-notes ans/reference-notes
	org-ref-default-bibliography '(ans/reference-bibfile)
	org-ref-pdf-directory ans/reference-dir-pdfs))

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
