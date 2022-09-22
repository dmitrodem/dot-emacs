(require 'centaur-tabs)
(global-set-key (kbd "C-<prior>")  'centaur-tabs-backward)
(global-set-key (kbd "C-<next>") 'centaur-tabs-forward)
(centaur-tabs-headline-match)
(setq centaur-tabs-style "bar")
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-set-bar 'over)
;; (setq centaur-tabs-set-modified-marker t)
(setq centaur-tabs-height 48)

(defun centaur-tabs-buffer-groups ()
  "`centaur-tabs-buffer-groups' control buffers' group rules.

    Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
    All buffer name start with * will group to \"Emacs\".
    Other buffer group by `centaur-tabs-get-group-name' with project name."
  (list
   (cond
    ((or (string-equal "*" (substring (buffer-name) 0 1))
	 (memq major-mode '(magit-process-mode
			    magit-status-mode
			    magit-diff-mode
			    magit-log-mode
			    magit-file-mode
			    magit-blob-mode
			    magit-blame-mode
			    )))
     "Emacs")
    ((derived-mode-p 'prog-mode)
     "Editing")
    ((derived-mode-p 'dired-mode)
     "Dired")
    ((memq major-mode '(helpful-mode
			help-mode))
     "Help")
    ((memq major-mode '(org-mode
			org-agenda-clockreport-mode
			org-src-mode
			org-agenda-mode
			org-beamer-mode
			org-indent-mode
			org-bullets-mode
			org-cdlatex-mode
			org-agenda-log-mode
			diary-mode))
     "OrgMode")
    (t
     (centaur-tabs-get-group-name (current-buffer))))))
;;(centaur-tabs-mode t)
