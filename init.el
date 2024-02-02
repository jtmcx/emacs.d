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
(setq-default display-line-numbers-width-start 100)
(setq-default display-line-numbers-grow-only t)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Show column number in the mode-line; hide percentage.
(setq mode-line-percent-position nil)
(column-number-mode)

;; Display a fill column at 80 characters.
(setq-default display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(defvar load-theme-hook nil
  "Hook run after `M-x load-theme'.")

;; Modify `load-theme' to run hooks when finished.
(advice-add 'load-theme :after
  (lambda (&rest _) (run-hooks 'load-theme-hook)))

;; By default, themes *cannot* be modified at runtime. Disabling this
;; option allows the use of `custom-theme-set-*` functions during init.
(setq custom--inhibit-theme-enable nil)

(defun catppuccin-theme-alterations ()
  "Apply alterations to the default catppuccin theme."
  (let ((ctp-crust    (catppuccin-get-color 'crust))
        (ctp-surface0 (catppuccin-get-color 'surface0))
        (ctp-surface1 (catppuccin-get-color 'surface1))
        (ctp-surface2 (catppuccin-get-color 'surface2)))
    (custom-theme-set-faces 'catppuccin
     ;; In my opinion, the modeline doesn't have enough contrast,
     ;; especially when `solaire-mode` is active.
     `(mode-line
	((t (:box        nil
             :overline   ,ctp-surface0
             :underline  ,ctp-surface0
             :background ,ctp-crust))))
     `(mode-line-inactive
	((t (:box        nil
             :overline   ,ctp-surface0
             :underline  ,ctp-surface0
             :background ,ctp-crust))))

     ;; Configure colors for Proof General.
     `(proof-locked-face ((t (:background ,ctp-surface1))) t)
     `(proof-queue-face  ((t (:background ,ctp-surface2))) t))))

(use-package catppuccin-theme
  :custom
  (catppuccin-italic-comments t)
  (catppuccin-enlarge-headings nil)
  (catppuccin-flavor 'mocha)
  :config
  (add-hook 'load-theme-hook #'catppuccin-theme-alterations)
  (load-theme 'catppuccin :no-confirm))

(use-package solaire-mode
  :config
  (solaire-global-mode))

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minions
  :init
  (minions-mode))

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
  (which-key-mode))

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
