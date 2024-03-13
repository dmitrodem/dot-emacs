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

;; fucking underscore
(add-hook 'prog-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

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

(require 'bind-key)

(use-package company)

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

(use-package powerline
  ;; :define powerline-default-theme
  :when (display-graphic-p)
  :init
  (powerline-default-theme))

(use-package tramp
  :custom
  (tramp-chunksize 500)
  (tramp-inline-compress-start-size 1000000)
  (tramp-copy-size-limit 1000000))

(use-package projectile)

(use-package lsp-mode
  :after (company-mode)
  :init
  (setq gc-cons-threshold 100000000
        company-minimum-prefix-length 1
        company-idle-delay 0.0
        read-process-output-max (* 1024 1024)))


(use-package highlight-symbol
  :commands (highlight-symbol
             highlight-symbol-next
             highlight-symbol-prev)
  :bind (("C-<f3>" . 'highlight-symbol)
         ("<f3>"   . 'highlight-symbol-next)
         ("S-<f3>" . 'highlight-symbol-prev)))

(use-package vala-mode
  :custom
  (vala-multiline-strings t)
  :config
  (setq indent-tabs-mode nil
        c-basic-offset 2
        tab-width 2))

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

(use-package bison-mode)

(use-package yasnippet
  :commands yas-global-mode
  :config
  (yas-global-mode t)
  :bind (
         :map yas-minor-mode-map
              ("C-c k" . yas-expand))
  )

(use-package magit)

(use-package expand-region
  :commands er/expand-region
  :bind (("C-=" . 'er/expand-region)))

(use-package whitespace-cleanup-mode
  :hook prog-mode)

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

(use-package all-the-icons-ibuffer
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))
(use-package ibuffer
  :ensure nil
  :bind (("C-x C-b" . ibuffer)))

(use-package flycheck
  :init
  ;; (global-flycheck-mode)
  )

(use-package helm
  :commands
  helm-ff-icon-mode
  :init
  (helm-ff-icon-mode t)
  :bind (("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)))

(use-package pdf-tools)

(use-package winum)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package yaml-mode)
(use-package meson-mode)

(use-package dracula-theme
  :defer nil
  :config
  (load-theme 'dracula t))

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

;; (use-package verilog-ts-mode
;;   :straight t
;;   :mode (("\\.s?vh?\\'" . verilog-ts-mode))
;;   )

;; (use-package vhdl-ext
;;   :straight t
;;   :after vhdl-mode
;;   :demand
;;   :hook ((vhdl-mode . vhdl-ext-mode))
;;   :init
;;   ;; Can also be set through `M-x RET customize-group RET vhdl-ext':
;;   ;;  - Vhdl Ext Feature List (provides info of different features)
;;   ;; Comment out/remove the ones you do not need
;;   (setq vhdl-ext-feature-list
;;         '(font-lock
;;           hierarchy
;;           eglot
;;           lsp
;;           flycheck
;;           beautify
;;           navigation
;;           template
;;           compilation
;;           imenu
;;           which-func
;;           hideshow
;;           time-stamp
;;           company-keywords
;;           ports))
;;   :config
;;   (vhdl-ext-mode-setup))

;; ;; To use `vhdl-ts-mode' as the default major-mode also add the lines below:
;; (use-package vhdl-ts-mode
;;   :straight t
;;   :mode (("\\.vhdl?\\'" . vhdl-ts-mode)))

(use-package systemrdl-mode
  :straight (systemrdl-mode
             :type git
             :host github
             :repo "paul-donahue/systemrdl-mode")
  :mode (("\\.rdl$" . systemrdl-mode)))

(use-package scad-mode)

(use-package fpga
  :init
  (setq fpga-feature-list '(xilinx)))

(use-package pixel-scroll
  :defer t
  :ensure nil
  :custom
  (pixel-scroll-precision-interpolate-mice t)
  (pixel-scroll-precision-interpolate-page t)
  (pixel-scroll-precision-large-scroll-height 50.0)
  :config
  (pixel-scroll-precision-mode t))

(use-package anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :after company
  :init
  (add-to-list 'company-backends 'company-anaconda))

(use-package company
  :hook ((python-mode . company-mode)))

(use-package pyvenv
  :hook ((python-mode . pyvenv-mode)))

(use-package dts-mode)
(use-package arduino-mode)

(provide '.emacs)
;;; .emacs ends here
