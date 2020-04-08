(require 'sr-speedbar)
(setq sr-speedbar-auto-refresh nil)
(setq sr-speedbar-right-side nil)
(setq ezimage-use-images t)
(setq speedbar-use-images t)
(add-hook 'after-make-frame-functions
	  (lambda(frame) (when window-system 
			       (clear-image-cache)
			       (message "clear-image-cache"))))
			   
