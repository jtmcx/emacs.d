;; -*- lexical-binding: t; coding: utf-8 -*-

(add-to-list 'load-path
  (expand-file-name "lisp" user-emacs-directory))

(load "init-elpaca.el")
(load "init-meow.el")

(defun font-exists-p (font)
  "Check if a FONT exists."
  (not (null (x-list-fonts font))))

(if (font-exists-p "mononoki")
  (set-frame-font "mononoki 13" nil t))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Show an empty scratch buffer on startup.
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")

;; Line numbers with a gutter wide enough for three digits.
(setq display-line-numbers-width-start 100)
(setq display-line-numbers-grow-only t)
(global-display-line-numbers-mode)

;; Display a fill column at 80 characters.
(setq display-fill-column-indicator-column 80)
(global-display-fill-column-indicator-mode)

(use-package catppuccin-theme
  :custom
  (catppuccin-italic-comments t)
  (catppuccin-enlarge-headings nil)
  :config
  (load-theme 'catppuccin :no-confirm))

(use-package solaire-mode
  :config
  (solaire-global-mode))

(use-package vi-tilde-fringe
  :hook prog-mode)

(use-package vertico
  :init
  (vertico-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
    '((file (styles basic partial-completion)))))

(use-package which-key
  :config
  (which-key-mode)
  (which-key-setup-side-window-right))

(use-package org)

(use-package flycheck
  :hook prog-mode)

(use-package company
  :hook prog-mode)

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-l")
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :commands (lsp lsp-deferred))

(use-package lsp-ui)

(use-package envrc
  :config
  (envrc-global-mode))

(load "lang-coq.el")
(load "lang-haskell.el")
(load "lang-ocaml.el")
(load "lang-prolog.el")

(elpaca-wait)
