(setq python-shell-interpreter "/usr/bin/ipython3")
(setq python-shell-interpreter-args "--simple-prompt -i")
(defun my/python-mode-hook ()
  (require 'company)
  (add-to-list 'company-backends 'company-jedi)
  (company-mode t))
  ;; (elpy-enable))

(add-hook 'python-mode-hook 'my/python-mode-hook)
