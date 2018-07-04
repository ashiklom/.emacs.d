;;; init-utils -- Various utility functions.
;;;
;;; Commentary:
;;;
;;; Code:
(defun rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(evil-ex-define-cmd "rename" 'rename-this-buffer-and-file)

(defun ans-switch-to-mru-buffer ()
  "Switch to most-recently-used (MRU) buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun ans--reload-initfile ()
  "Reload the Emacs init file."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(defun ans--edit-initfile ()
  "Edit the Emacs init file."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defvar ans-window-minimized '()
  "Configuration of currently minimized windows.
See `ans-toggle-minimize'.")

(defun ans-toggle-minimize ()
  "Toggle the maximization state of a window."
  (interactive)
  (if ans-window-minimized
      (progn (set-window-configuration (pop ans-window-minimized))
	     (message "Windows restored."))
    (progn (push (current-window-configuration) ans-window-minimized)
	   (delete-other-windows)
	   (message "Window minimized."))
    ))

(defun ans/delete-file-and-buffer ()
  "Kill the current buffer and delete the associated file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (progn
	(delete-file filename)
	(message "Deleted file %s" filename)
	(kill-buffer)))))

(defun ans/pop-window-into-frame ()
  "Pop current window into its own frame."
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

(evil-ex-define-cmd "dkill" 'ans/delete-file-and-buffer)

(use-package dired-single
  :ensure t
  :config
  (general-def
    :keymaps 'dired-mode-map
    "RET" 'dired-single-buffer
    "^" (lambda () (interactive) (dired-single-buffer ".."))))

(provide 'init-utils)
;;; init-utils ends here
