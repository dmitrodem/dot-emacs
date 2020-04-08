;; (setq dired-omit-files "\\|^\\..+$")
(setq dired-dwim-target t)
(setq dired-omit-mode t)

(defun xah-dired-mode-setup ()
  "to be run as hook for `dired-mode'."
  (dired-hide-details-mode 1))
(add-hook 'dired-load-hook '(lambda () (require 'dired-x)))
(add-hook 'dired-mode-hook 'xah-dired-mode-setup)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(message "dired")
