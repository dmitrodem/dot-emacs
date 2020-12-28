;; (setq dired-omit-files "\\|^\\..+$")
(setq dired-dwim-target t)
(setq dired-omit-mode t)

(defun xah-dired-mode-setup ()
  "to be run as hook for `dired-mode'."
  (dired-hide-details-mode 1))
(add-hook 'dired-load-hook '(lambda () (require 'dired-x)))
(add-hook 'dired-mode-hook 'xah-dired-mode-setup)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(defun my-dired-find-parent-directory ()
  (interactive)
  (find-alternate-file ".."))

(defun my-dired-find-file ()
  (interactive)
  (let ((d (dired-get-file-for-visit)))
    (if (file-directory-p d)
        (find-alternate-file d)
      (find-file-other-window d))
    ))

(add-hook 'dired-load-hook
          '(lambda ()
             (define-key dired-mode-map (kbd "<left>") 'my-dired-find-parent-directory)
             (define-key dired-mode-map (kbd "<right>") 'my-dired-find-file)))

(message "dired")
