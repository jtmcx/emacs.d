;; -*- lexical-binding: t; coding: utf-8 -*-

;; Note: I used to configure the font here, but it's much easier to let
;; the OS handle it instead. For example, in a gnome environment:
;;
;;   $ gsettings set org.gnome.desktop.interface \
;;             monospace-font-name 'Jet Brains Mono 13'

(add-to-list 'load-path
  (expand-file-name "lisp" user-emacs-directory))

(load "setup-elpaca.el")
(load "setup-meow.el")

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

(defvar load-theme-hook nil
  "Hook run after `M-x load-theme'.")

;; Modify `load-theme' to run hooks when finished.
(advice-add 'load-theme :after
  (lambda (&rest _) (run-hooks 'load-theme-hook)))

(defun catppuccin-proof-mode-theme ()
  "Configure Proof General faces with catppuccin colors."
  (let ((ctp-surface1 (catppuccin-get-color 'surface1))
        (ctp-surface2 (catppuccin-get-color 'surface2)))
  (custom-set-faces `(proof-locked-face ((t (:background ,ctp-surface1)))))
  (custom-set-faces `(proof-queue-face  ((t (:background ,ctp-surface2)))))))

(use-package catppuccin-theme
  :custom
  (catppuccin-italic-comments t)
  (catppuccin-enlarge-headings nil)
  (catppuccin-flavor 'mocha)
  :config
  (add-hook 'load-theme-hook #'catppuccin-proof-mode-theme)
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
