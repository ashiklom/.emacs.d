;;; init-evil -- Configuration for evil
;;;
;;; Commentary:
;;;
;;; Code:

;; General package for better key-bindings
(use-package general
  :ensure t)

;; Evaluate lisp at point
;; Define this early to facilitate debugging
(general-def
  :keymaps 'lisp-mode-shared-map
  :states '(motion insert)
  "<C-return>" 'eval-defun)

(use-package evil
  :ensure t
  :demand
  :config
  (evil-mode)
  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))
  (use-package evil-indent-textobject
    :ensure t)
  (use-package evil-embrace
    :ensure t
    :init
    (setq evil-embrace-show-help-p nil)
    :config
    (evil-embrace-enable-evil-surround-integration)
    (add-hook 'org-mode-hook 'embrace-org-mode-hook)
    (add-hook 'LaTeX-mode-hook 'embrace-LaTeX-mode-hook))
  ;; Don't use evil mode in these, but enable some Evil mappings
  (add-to-list 'evil-emacs-state-modes 'help-mode)
  (add-to-list 'evil-emacs-state-modes 'messages-buffer-mode)
  (add-to-list 'evil-emacs-state-modes 'special-mode)
  ;; (general-def
  ;;   :keymaps '(help-mode-map
  ;; 	       messages-buffer-mode-map
  ;; 	       special-mode-map)
  ;;   "h" 'evil-backward-char
  ;;   "l" 'evil-forward-char
  ;;   "k" 'evil-previous-visual-line
  ;;   "j" 'evil-next-visual-line
  ;;   "C-u" 'evil-scroll-up
  ;;   "C-d" 'evil-scroll-down
  ;;   "/" 'evil-search-forward
  ;;   "n" 'evil-search-next
  ;;   "N" 'evil-search-previous
  ;;   )
  ;; Some special mappings for dired mode
  (add-to-list 'evil-emacs-state-modes 'dired-mode)
  (general-def
    :keymaps '(dired-mode-map)
    :states '(emacs)
    "E" 'find-file)
  )

(general-unbind '(motion normal visual)
  "SPC"
  "C-u"
  "\\")
(general-unbind "M-SPC")

(general-def
  :states 'insert
  "j" (general-key-dispatch 'self-insert-command
	:timeout 0.25
	"k" 'evil-normal-state)
  )

(general-def
  :states '(motion normal visual)
  ;; Move by visual lines
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line
  "gj" 'evil-next-line
  "gk" 'evil-previous-line
  "C-=" 'evil-window-increase-height
  "C--" 'evil-window-decrease-height
  "C-+" 'evil-window-increase-width
  "C-_" 'evil-window-decrease-width
  "C-0" 'balance-windows
  "C-)" 'shrink-window-if-larger-than-buffer
  "C-d" 'evil-scroll-down
  "C-u" 'evil-scroll-up
  )

(general-def
  :states 'normal
  "S" 'save-buffer)

(general-create-definer ans-leader-def
  :prefix "SPC"
  :non-normal-prefix "M-SPC")
(ans-leader-def
  :states '(motion normal visual emacs)
  :keymaps 'override
  "b" 'helm-mini
  "o" 'other-window
  "j" 'evil-window-down
  "k" 'evil-window-up
  "h" 'evil-window-left
  "l" 'evil-window-right
  "0" 'delete-other-windows
  "\\" 'evil-window-vsplit
  "-" 'evil-window-split
  "<up>" 'buf-move-up
  "<down>" 'buf-move-down
  "<left>" 'buf-move-left
  "<right>" 'buf-move-right
  "O" 'other-frame
  "E" 'new-frame
  "TAB" 'ans-switch-to-mru-buffer
  "." 'evil-next-buffer
  "," 'evil-prev-buffer
  ":" 'eval-expression
  "d" 'dired
  "x" 'helm-M-x
  "q" 'kill-this-buffer
  "Q" 'evil-quit
  "sv" 'ans--reload-initfile
  "ev" 'ans--edit-initfile
  "sx" (lambda() (interactive)(switch-to-buffer "*scratch*"))
  "ws" 'toggle-truncate-lines
  "ss" 'delete-trailing-whitespace
  "L" 'whitespace-mode			; Show all whitespace characters
  "z" 'ans-toggle-minimize
  "'" 'comment-dwim			; Insert right comment
  "ea" 'align-regexp
  "*" 'universal-argument		; Emacs's C-u
  "C" 'org-capture
  "ee" 'sr-speedbar-toggle
  )

(use-package evil-nerd-commenter
  :ensure t
  :general
  (ans-leader-def
    :states '(normal visual)
    ";" 'evilnc-comment-or-uncomment-lines
    "\"" 'evilnc-comment-or-uncomment-paragraphs
    )
  )

(use-package ace-jump-mode
  :ensure t
  :general
  (ans-leader-def
    :states '(motion normal emacs)
    :infix "SPC"
    "w" 'evil-ace-jump-word-mode
    "l" 'evil-ace-jump-line-mode
    "f" 'evil-ace-jump-char-mode
    "t" 'evil-ace-jump-char-to-mode))

(provide 'init-evil)
;;; init-evil ends here
