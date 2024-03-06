;; -*- lexical-binding: t; coding: utf-8 -*-

;; Disable package.el (we're using elpaca).
(setq package-enable-at-startup nil)

;; No UI fluff.
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Show an empty scratch buffer on startup.
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")

;;; early-init.el ends here
