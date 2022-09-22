(defun my-vala-mode-hook ()
  "Custom style for vala-mode"
  (interactive)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 2)
  (setq tab-width 2)
  )

(add-hook 'vala-mode-hook 'my-vala-mode-hook)

                                      
