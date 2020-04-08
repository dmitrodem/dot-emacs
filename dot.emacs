(switch-to-buffer "*Messages*")
(autoload 'my-site-start "~/.emacs.d/packages/my-site-start.el" nil t)
(my-site-start "~/.emacs.d/site-start.d/")

(with-current-buffer " *load*"
  (goto-char (point-max)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '()))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
