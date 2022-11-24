(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(message "packages")

(defvar my-packages
  '(
    material-theme
    zenburn-theme
    company
    elpy
    company-jedi
    company-lsp
    sr-speedbar
    all-the-icons-dired
    yasnippet
    lsp-mode
    lsp-ui
    auctex
    auctex-latexmk
    neotree
    magit
    helm
    helm-descbinds
    helm-projectile
    projectile
    highlight-symbol
    centaur-tabs
    kconfig-mode
    powerline
    bison-mode
    spice-mode
    nhexl-mode
    yaml-mode
    vala-mode
    jinja2-mode
    rnc-mode
    scala-mode
    sbt-mode
    magit
    meson-mode
    ))

(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      my-packages)

(add-to-list 'load-path "~/.emacs.d/packages/")
(add-to-list 'load-path "~/.emacs.d/packages/telega.el")
(require 'telega)
