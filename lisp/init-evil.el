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
  :init
  (setq evil-want-integration nil)
  :config
  (evil-mode)
  (defalias #'forward-evil-word #'forward-evil-symbol))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode))

(use-package evil-collection
  :ensure t
  :after evil
  :init
  (setq evil-collection-company-use-tng nil)
  :config
  (setq evil-collection-mode-list (remove 'company evil-collection-mode-list))
  (evil-collection-init))

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

;; (general-def
;;   :keymaps '(override evil-org-mode-map org-mode-map)
;;   "M-h" 'evil-window-left
;;   "M-l" 'evil-window-right
;;   "M-k" 'evil-window-up
;;   "M-j" 'evil-window-down)

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

(general-def
  :states '(motion normal)
  :prefix "C-c C-s"
  "c" 'slack-channel-select
  "m" 'slack-im-select
  "t" 'slack-thread-select
  "u" 'slack-select-unread-rooms
  "r" 'slack-select-rooms)

(general-create-definer ans-leader-def
:prefix "SPC"
:non-normal-prefix "M-SPC"
:prefix-command 'ans-leader-command
:prefix-map 'ans-leader-map)

(ans-leader-def
  :states '(motion normal visual emacs)
  :keymaps 'override
  "b" 'helm-mini
  "f" 'helm-find-files
  "\\" 'evil-window-vsplit
  "-" 'evil-window-split
  "+" 'make-frame-command
  "<up>" 'buf-move-up
  "<down>" 'buf-move-down
  "<left>" 'buf-move-left
  "<right>" 'buf-move-right
  ":" 'eval-expression
  "dd" 'dired
  "dw" 'dired-other-window
  "df" 'dired-other-frame
  "x" 'helm-M-x
  "sv" 'ans--reload-initfile
  "sx" (lambda() (interactive)(switch-to-buffer "*scratch*"))
  "ss" 'delete-trailing-whitespace
  "'" 'comment-dwim			; Insert right comment
  "*" 'universal-argument		; Emacs's C-u
  "C" 'org-capture
  "Y" 'org-store-link
  "L" 'org-insert-last-stored-link
  "vl" 'visual-line-mode
  "@" 'org-agenda
  "ww" 'quit-window
  "wW" 'kill-this-buffer
  "wd" 'delete-window
  "wD" 'kill-buffer-and-window
  "/" 'helm-occur
  "\"" 'helm-show-kill-ring)

(ans/add-evil-maps 'occur-mode-map)

(use-package evil-nerd-commenter
  :ensure t
  :general
  (ans-leader-def
    :states '(normal visual)
    ";" 'evilnc-comment-or-uncomment-lines))

(use-package evil-easymotion
  :ensure t)
(evilem-default-keybindings "SPC SPC")

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
  :diminish
  :config
  (evil-exchange-install))

(use-package evil-numbers
  :ensure t
  :after evil
  :diminish
  :init
  (defhydra evil-numbers-hydra ()
    "Increment or decrement numbers."
    ("=" evil-numbers/inc-at-pt "Increment")
    ("-" evil-numbers/dec-at-pt "Decrement"))
  :general
  (general-def
    :states 'normal
    "C-a" 'evil-numbers-hydra/body))

(provide 'init-evil)
;;; init-evil ends here
