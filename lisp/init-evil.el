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
  ;; Don't use evil mode in these, but enable some Evil mappings
  (add-to-list 'evil-emacs-state-modes 'help-mode)
  (add-to-list 'evil-emacs-state-modes 'messages-buffer-mode)
  (add-to-list 'evil-emacs-state-modes 'special-mode)
  ;; Some special mappings for dired mode
  (add-to-list 'evil-emacs-state-modes 'dired-mode)
  (general-def
    :keymaps '(dired-mode-map)
    :states '(emacs)
    "E" 'find-file)
  ;; Treat symbols as evil words
  (defalias #'forward-evil-word #'forward-evil-symbol)
  )

(use-package evil-indent-textobject
  :ensure t
  :after evil)

(use-package evil-embrace
  :ensure t
  :after evil
  :init
  (setq evil-embrace-show-help-p nil)
  :config
  (evil-embrace-enable-evil-surround-integration)
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (add-hook 'LaTeX-mode-hook 'embrace-LaTeX-mode-hook))

(defun ans/add-evil-maps (keymap)
  "Add some basic navigation mappings (including hjkl) to KEYMAP."
  (general-def
    :keymaps keymap
    "h" 'evil-backward-char
    "l" 'evil-forward-char
    "k" 'evil-previous-visual-line
    "j" 'evil-next-visual-line
    "C-u" 'evil-scroll-up
    "C-d" 'evil-scroll-down
    "/" 'evil-search-forward
    "n" 'evil-search-next
    "N" 'evil-search-previous
    "C-w C-w" 'ace-window))

(ans/add-evil-maps 'help-mode-map)

(general-def
  :keymaps '(override evil-org-mode-map org-mode-map)
  "M-h" 'evil-window-left
  "M-l" 'evil-window-right
  "M-k" 'evil-window-up
  "M-j" 'evil-window-down)

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
  :non-normal-prefix "M-SPC"
  :prefix-command 'ans-leader-command
  :prefix-map 'ans-leader-map)

(ans-leader-def
  :states '(motion normal visual emacs)
  :keymaps 'override
  "b" 'helm-mini
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
  "d" 'dired-other-window
  "D" 'dired-other-frame
  "x" 'helm-M-x
  "q" 'kill-this-buffer
  "sv" 'ans--reload-initfile
  "ev" 'ans--edit-initfile
  "sx" (lambda() (interactive)(switch-to-buffer "*scratch*"))
  "ss" 'delete-trailing-whitespace
  "z" 'ans-toggle-minimize
  "'" 'comment-dwim			; Insert right comment
  "ea" 'align-regexp
  "*" 'universal-argument		; Emacs's C-u
  "C" 'org-capture
  "Y" 'org-store-link
  "L" 'org-insert-last-stored-link
  "@" 'org-agenda
  "ee" 'sr-speedbar-toggle)

(ans/add-evil-maps 'occur-mode-map)

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
    "SPC w" 'evil-ace-jump-word-mode
    "SPC l" 'evil-ace-jump-line-mode
    "SPC f" 'evil-ace-jump-char-mode
    "SPC t" 'evil-ace-jump-char-to-mode))

(use-package ace-window
  :ensure t
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :commands ace-window
  :general
  (general-def "M-o" 'ace-window))

(use-package evil-exchange
  :ensure t
  :after evil
  :config
  (evil-exchange-install))

(provide 'init-evil)
;;; init-evil ends here
