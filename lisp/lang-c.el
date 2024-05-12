;; -*- lexical-binding: t; coding: utf-8 -*-

(setq-default c-basic-offset 8)

(use-package clang-format
  :config
  (add-hook 'c-mode-hook
    (lambda () (fset 'c-indent-region 'clang-format-region))))

(use-package cmake-mode)

(use-package ccls
  :config
  (add-hook 'c++-mode-hook #'lsp))

