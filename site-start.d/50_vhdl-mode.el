(require 'vhdl-mode)
(require 'lsp-mode)
(setq vhdl-array-index-record-field-in-sensitivity-list t)
(setq vhdl-company-name "MIPT")
(setq vhdl-compiler "ModelSim")
(setq vhdl-copyright-string "Copyright (c) MIPT")
(setq vhdl-file-header
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
(setq vhdl-standard (quote (93 nil)))
;; (require 'compile)
;; (add-to-list 'compilation-error-regexp-alist '("** Error: \\(.+\\)(\\([0-9]*\\)):" 1 2))
;; (setq vhdl-compile-use-local-error-regexp t)
(setq vhdl-project-auto-load nil)
(setq vhdl-modify-date-prefix-string "--! @modified  ")
(setq vhdl-modify-date-on-saving t)
(setq vhdl-highlight-translate-off t)
(add-hook 'vhdl-mode-hook (lambda ()
                            (progn
                              ;; (require 'company)
                              ;; (require 'company-lsp)
                              ;; (add-to-list 'company-backends 'company-lsp)
                              (add-hook 'before-save-hook 'whitespace-cleanup))))
;(setq lsp-ui-doc-position 'top)
;(setq lsp-restart 'auto-restart)
;(setq lsp-vhdl-server 'vhdl-tool)
;(setq lsp-vhdl-server-path "/usr/local/bin/vhdl-tool")
;(add-hook 'vhdl-mode-hook 'lsp)
(defun vhdl-expand-auto (start end)
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
                  ))
            (forward-line 1))))))
(define-key vhdl-mode-map (kbd "C-c M-a") 'vhdl-expand-auto)
