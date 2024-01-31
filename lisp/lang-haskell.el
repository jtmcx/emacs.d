;; -*- lexical-binding: t; coding: utf-8 -*-

;; lsp-haskell hooks into lsp-deferred instead of lsp.
;; See https://github.com/purcell/envrc/issues/31

(use-package lsp-haskell
  :config
  (add-hook 'haskell-mode-hook #'lsp-deferred)
  (add-hook 'haskell-literate-mode-hook #'lsp-deferred))
