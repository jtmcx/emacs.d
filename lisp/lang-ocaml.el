;; -*- lexical-binding: t; coding: utf-8 -*-

(use-package tuareg
  :config
  (add-hook 'tuareg-mode-hook #'lsp))

(use-package dune)

(use-package ocp-indent)
