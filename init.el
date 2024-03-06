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

;; Toggle menu-bar with <f10>.
(global-set-key (kbd "<f10>") 'menu-bar-mode)

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

(windmove-default-keybindings 'meta)

;; Theme
;; ----------------------------------------------------------------------------

(defvar load-theme-hook nil
  "Hook run after `M-x load-theme'.")

;; Modify `load-theme' to run hooks when finished.
(advice-add 'load-theme :after
  (lambda (&rest _) (run-hooks 'load-theme-hook)))

(use-package catppuccin-theme
  :custom
  (catppuccin-italic-comments t)
  (catppuccin-enlarge-headings nil)
  (catppuccin-flavor 'macchiato)
  :config
  (add-hook 'load-theme-hook #'catppuccin-theme-alterations)
  (load-theme 'catppuccin :no-confirm))

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

     ;; Configure colors for keycast.
     `(keycast-key ((t (:background ,ctp-surface0))))

     ;; Lower the contrast for the fill-column-indicator
     `(fill-column-indicator ((t (:foreground ,ctp-surface1))))

     ;; Configure colors for Proof General.
     `(proof-locked-face ((t (:background ,ctp-surface1))) t)
     `(proof-queue-face  ((t (:background ,ctp-surface2))) t))))

(defun my/dark-mode ()
  "Toggle light and dark catppuccin themes."
  (interactive)
  (when (member 'catppuccin custom-enabled-themes)
    (if (eq catppuccin-flavor 'latte)
        (catppuccin-load-flavor 'macchiato)
      (catppuccin-load-flavor 'latte))))

(global-set-key (kbd "<f9>") 'my/dark-mode)

(use-package solaire-mode
  :config (solaire-global-mode))

(use-package vi-tilde-fringe
  :hook prog-mode)

;; Mode-line Configuration
;; ----------------------------------------------------------------------------

(use-package moody
  ;; Use a custom fork of moody that is aware of solaire-mode.
  :elpaca '(moody :host github
		  :repo "jtmcx/moody"
		  :branch "solaire-mode")
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minions
  :init (minions-mode))

(use-package keycast
  :config
  ;; Someday it would be nice for keycast to be wrapped in a moody ribbon.
  (setq keycast-mode-line-format "%2s%k%c%R")
  (setq keycast-mode-line-remove-tail-elements nil)
  (setq keycast-mode-line-insert-after 'minions-mode-line-modes))

;; Completion Interface
;; ----------------------------------------------------------------------------

(use-package vertico
  :init (vertico-mode))

(use-package marginalia
  :init (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
    '((file (styles basic partial-completion)))))

(use-package which-key
  :config (which-key-mode))

;; Developer Tools
;; ----------------------------------------------------------------------------

(use-package anzu)

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

(use-package magit)

(use-package ws-butler
  :hook prog-mode)

;; Language Specific Configuration
;; ----------------------------------------------------------------------------

(load "lang-coq.el")
(load "lang-haskell.el")
(load "lang-ocaml.el")
(load "lang-prolog.el")
(load "lang-racket.el")

;; Org-mode
;; ----------------------------------------------------------------------------

(use-package org)
(setq org-src-preserve-indentation 1)

(use-package htmlize)

(elpaca-wait)
