;; -*- lexical-binding: t; coding: utf-8 -*-

(use-package proof-general
  ;; Use a fork of proof-general that has support for abella.
  ;; See https://abella-prover.org/reference-guide.html#emacs
  :elpaca
  (proof-general
   :host github
   :repo "abella-prover/PG"
   :branch "abella"
   ;; Add abella to list of files. This is a modification of the upstream
   ;; recipe: https://github.com/melpa/melpa/blob/master/recipes/proof-general
   :files  (:defaults "CHANGES" "AUTHORS" "COPYING"
                      "generic" "images" "lib"
                      ("coq" "coq/*.el")
                      "easycrypt" "phox" "qrhl"
                      "pghaskell" "pgocaml" "pgshell"
		      ("abella" "abella/abella.el" "abella/abella-syntax.el")))
  :custom
  (proof-splash-enable nil))
