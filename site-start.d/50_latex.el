(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq LaTeX-electric-left-right-brace t)
(setq TeX-PDF-mode t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
