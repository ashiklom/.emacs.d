;;; init-yasnippet -- My configuration for Yasnippet
;;;
;;; Commentary:
;;;
;;; Code:
(use-package yasnippet
  :ensure t
  :demand
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"
			   "~/.emacs.d/remote-snippets"))
  :config
  (yas-reload-all)
  (yas-global-mode 1)
  ;; Use something like this if you don't want snippets globally:
  ;; (yas-reload-all)
  ;; (add-hook 'prog-mode-hook #'yas-minor-mode)
  :general
  (general-def
    :keymaps 'yas-minor-mode-map
    "<escape>" 'yas-exit-snippet)
  (ans-leader-def
    :states '(motion normal)
    "un" 'yas-new-snippet
    "ue" 'yas-visit-snippet-file)
  )

(provide 'init-yasnippet)
;;; init-yasnippet ends here
