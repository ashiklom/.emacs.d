;;; init.el --- Alexey Shiklomanov's Emacs init file
;;;
;;; Commentary:
;;;
;;; Code:
;; Use Emacs's internal package manager
(let ((file-name-handler-alist nil)
      (my-emacs-file (expand-file-name "emacs.elc" user-emacs-directory)))
  ;; If config is precompiled, load it
  (if (file-exists-p my-emacs-file)
      (load-file my-emacs-file)
    ;; Otherwise, use org-babel to tangle the file and then load.
    (require 'org)
    (org-babel-load-file (expand-file-name "configuration.org" user-emacs-directory))))

(provide 'init)
;;; init.el ends here
