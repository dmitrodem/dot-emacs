(setq custom-file "~/.config/emacs/customize.el")
(when (file-exists-p custom-file)
 (load-file custom-file))

;; bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; GUI setup
(savehist-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t
      inhibit-startup-message t)

(defvar my/mainfont
  "Fira Code-10"
  "Default font")

(set-frame-font my/mainfont)

(add-to-list 'default-frame-alist `(font . ,my/mainfont))
(set-fontset-font t 'unicode "Material Icons" nil 'append)

;; editing options
(setq-default indent-tabs-mode nil)
(setq user-full-name    "Dmitriy Dyomin"
      user-mail-address "dmitrodem@gmail.com")
(keymap-global-set "<f10>" 'menu-bar-open)
(keymap-global-set "C-c C-c" 'comment-dwim)
(keymap-global-unset "C-z")

;; remote dir-locals.el
(setq enable-remote-dir-locals t)


;; package setup
(require 'package)
(setq package-archives '(
                         ("gnu"    . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa"  . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (straight-use-package 'use-package))

(eval-when-compile
  (progn
    (require 'use-package)
    ;; silence compilation warnings
    (defvar native-comp-async-report-warnings-errors 'silent)))

(setq
 use-package-always-ensure t
 use-package-always-defer t
 use-package-expand-minimally t)

;; use-package instances

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; system settings                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package tramp
  :ensure nil
  :custom
  (tramp-chunksize 500)
  (tramp-inline-compress-start-size 1000000)
  (tramp-copy-size-limit 1000000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; look and feel                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dracula-theme
  :defer nil
  :config
  (load-theme 'dracula t))

(use-package powerline
  :when (display-graphic-p)
  :init
  (powerline-default-theme))

(use-package pixel-scroll
  :ensure nil
  :custom
  (pixel-scroll-precision-interpolate-mice t)
  (pixel-scroll-precision-interpolate-page t)
  (pixel-scroll-precision-large-scroll-height 50.0)
  :init
  (pixel-scroll-precision-mode t))

(use-package all-the-icons-dired
  :hook dired-mode)
(use-package dired
  :ensure nil
  :init
  (put 'dired-find-alternate-file 'disabled nil)
  :bind (:map dired-mode-map
              ("<right>" . dired-find-alternate-file)
              ("<left>"  . dired-up-directory))
  :custom
  (dired-listing-switches "-l"))

(use-package all-the-icons)
(use-package all-the-icons-ibuffer
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))
(use-package ibuffer
  :ensure nil
  :bind (("C-x C-b" . ibuffer)))

(use-package helm
  :commands
  helm-ff-icon-mode
  :init
  (helm-ff-icon-mode t)
  :bind (("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)))

(use-package highlight-symbol
  :commands (highlight-symbol
             highlight-symbol-next
             highlight-symbol-prev)
  :bind (("C-<f3>" . 'highlight-symbol)
         ("<f3>"   . 'highlight-symbol-next)
         ("S-<f3>" . 'highlight-symbol-prev)))

(use-package expand-region
  :commands er/expand-region
  :bind (("C-=" . 'er/expand-region)))

(use-package yasnippet
  :commands yas-global-mode
  :config
  (yas-global-mode t)
  :bind (
         :map yas-minor-mode-map
              ("C-c k" . yas-expand)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prog-mode settings                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit)

(defun underscore-as-word-symbol ()
  (modify-syntax-entry ?_ "w"))

(use-package whitespace-cleanup-mode
  :hook
  (prog-mode . whitespace-cleanup-mode)
  (prog-mode . underscore-as-word-symbol))

(use-package projectile)

(use-package company
  :hook (python-mode))

(use-package lsp-mode
  :after (company-mode)
  :init
  (setq gc-cons-threshold 100000000
        company-minimum-prefix-length 1
        company-idle-delay 0.0
        read-process-output-max (* 1024 1024))
  :hook (python-mode verilog-mode))

(use-package lsp-ui)

(use-package flycheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VHDL                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my/vhdl-file-header
"-------------------------------------------------------------------------------
--! @file      <filename>
--! @brief     <title string>
--! @details   <description string>
--! @author    <author>
--! @date      <date>
--! @modified  <date>
--! @version   0.1
--! @copyright <copyright> <year>
-------------------------------------------------------------------------------
")

(defun my/vhdl-expand-auto (start end)
  (interactive "r")
  (if (use-region-p)
      (let ((cnt nil))
        (message "Expanding region")
        (save-excursion
          (goto-char start)
          (while (< (point) end)
            (beginning-of-line)
            (if (and cnt (search-forward ":=" (line-end-position) t nil))
                (let ((startpoint (point)))
                  (if (search-forward ";" (line-end-position) t nil)
                      (progn
                        (backward-char)
                        (kill-region startpoint (point))
                        (insert (format " %d" cnt))))))
            (beginning-of-line)
            (if (search-forward "--" (line-end-position) t nil)
                (let ((comment (buffer-substring (point) (line-end-position))))
                  (if (string-match "=\\([+-]?[0-9]+\\)" comment)
                      (setq cnt (string-to-number (match-string 1 comment))))
                  (if (and cnt (string-match "\\([+-][0-9]+\\)" comment))
                      (setq cnt (+ cnt (string-to-number (match-string 1 comment)))))
                  (message (format "cnt = %d" cnt))
                  ))
            (forward-line 1))))))

(use-package vhdl-mode
  :init
  (setq vhdl-array-index-record-field-in-sensitivity-list t
        vhdl-company-name "MIPT"
        vhdl-compiler "ModelSim"
        vhdl-copyright-string "Copyright (c) MIPT"
        vhdl-file-header my/vhdl-file-header
        vhdl-standard (quote (93 nil))
        vhdl-project-autoload nil
        vhdl-modify-date-prefix-string "--! @modified  "
        vhdl-modify-date-on-saving t
        vhdl-highlight-translate-off nil
        vhdl-electric-mode t
        vhdl-stutter-mode t)
  :custom
  (lsp-vhdl-server         'vhdl-tool)
  ;; (lsp-vhdl-server      'ghdl-ls)
  ;; (lsp-vhdl-server-path "~/.local/bin/ghdl-ls")
  (require 'lsp-vhdl)
  :bind (:map vhdl-mode-map
              ("C-c C-a" . my/vhdl-expand-auto))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; verilog                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package verilog-mode
  :init
  (setq
   verilog-align-ifelse t
   verilog-auto-delete-trailing-whitespace t
   verilog-auto-inst-param-value t
   verilog-auto-inst-vector nil
   verilog-auto-lineup (quote all)
   verilog-auto-newline nil
   verilog-auto-save-policy nil
   verilog-auto-template-warn-unused t
   verilog-case-indent 2
   verilog-cexp-indent 2
   verilog-highlight-grouping-keywords t
   verilog-highlight-modules t
   verilog-indent-level 2
   verilog-indent-level-behavioral 2
   verilog-indent-level-declaration 2
   verilog-indent-level-module 2
   verilog-tab-to-comment t
   verilog-indent-lists nil))

(use-package verilog-ext
  :straight t
  :after verilog-mode
  :demand
  :hook ((verilog-mode . verilog-ext-mode))
  :init
  (setq
   verilog-ext-feature-list
   '(font-lock
     xref
     capf
     hierarchy
     eglot
     lsp
     flycheck
     beautify
     navigation
     template
     formatter
     compilation
     imenu
     which-func
     hideshow
     typedefs
     time-stamp
     block-end-comments
     company-keywords
     ports))
  :config (verilog-ext-mode-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other EDA settings                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package fpga
  :init
  (setq fpga-feature-list '(xilinx)))

(use-package systemrdl-mode
  :straight (systemrdl-mode
             :type git
             :host github
             :repo "paul-donahue/systemrdl-mode")
  :mode (("\\.rdl$" . systemrdl-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :after company
  :init
  (add-to-list 'company-backends 'company-anaconda))

(use-package pyvenv
  :hook ((python-mode . pyvenv-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other prog-modes                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vala-mode
  :custom
  (vala-multiline-strings t)
  :config
  (setq indent-tabs-mode nil
        c-basic-offset 2
        tab-width 2))

(use-package bison-mode)
(use-package yaml-mode)
(use-package meson-mode)
(use-package dts-mode)
(use-package arduino-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CAD                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package scad-mode)
(provide '.emacs)
;;; .emacs ends here
