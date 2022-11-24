(savehist-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(set-frame-font "Fira Code-10")
(add-to-list 'default-frame-alist '(font . "Fira Code-10"))
;; (set-fontset-font t 'unicode "Symbola" nil 'append)
(set-fontset-font t 'unicode "Material Icons" nil 'append)

(setq native-comp-async-report-warnings-errors 'silent)
