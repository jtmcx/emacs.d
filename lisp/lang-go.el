;; -*- lexical-binding: t; coding: utf-8 -*-

(defun my/go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-mode
  :hook ((go-mode . lsp-deferred)
         (go-mode . my/go-install-save-hooks)))
