;; -*- lexical-binding: t; coding: utf-8 -*-

(use-package proof-general)

(use-package company-coq
  :hook coq-mode
  :custom
  (company-coq-disabled-features
    '(hello prettify-symbols spinner)))

;; The Proof General keybindings are fine, but I generally prefer the
;; keybindings in coqide (they're shorter). I've replicated the most
;; commonly used coqide keybindings here:

(add-hook 'coq-mode-hook
  (lambda ()	  
    (local-set-key (kbd "C-<down>")
		   #'proof-assert-next-command-interactive)
    (local-set-key (kbd "C-<up>")
		   #'proof-undo-last-successful-command)
    (local-set-key (kbd "C-<right>")
		   #'proof-goto-point)))
