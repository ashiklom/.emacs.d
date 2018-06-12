;; Stuff I don't use anymore

;; Visually-smooth scrolling
(use-package sublimity
  :ensure t
  :disabled
  :diminish
  :init
  (setq sublimity-scroll-weight 10
	sublimity-scroll-drift-length 5)
  :config
  (require 'sublimity-scroll)
  (sublimity-mode 1))
