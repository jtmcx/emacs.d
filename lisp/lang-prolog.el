;; -*- lexical-binding: t; coding: utf-8 -*-

(defun ediprolog-add-keys ()
  (local-set-key (kbd "C-c C-e") #'ediprolog-dwim)
  (local-set-key (kbd "C-c C-f") #'ediprolog-consult))

(use-package ediprolog
  :hook (prolog-mode . ediprolog-add-keys))
