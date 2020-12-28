(autoload 'spice-mode "spice-mode" "Spice/Layla Editing Mode" t)
(setq auto-mode-alist (append (list (cons "\\.sp$"  'spice-mode)
 				    (cons "\\.cir$" 'spice-mode)
 				    (cons "\\.cdl$" 'spice-mode)
 				    (cons "\\.chi$" 'spice-mode) ; output
 				    (cons "\\.mod$" 'spice-mode)); models
 			      auto-mode-alist))
