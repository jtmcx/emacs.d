(use-package meow
  :ensure t
  :demand t
  :config (setup-meow))

(defun setup-meow ()
  "Configure meow and enable it globally."

  (meow-setup-indicator)

  (setq meow-expand-hint-remove-delay 2.5)

  (meow-normal-define-key
   ;; Inserts
   '("a" . meow-append)
   '("i" . meow-insert)
   '("o" . meow-open-below)
   '("O" . meow-open-above)

   ;; Selects
   '("b" . meow-back-word)
   '("w" . meow-next-word)
   '("W" . meow-mark-word)
   '("f" . meow-find)
   '("t" . meow-till)
   '("x" . meow-line)
   '("z" . meow-pop-selection)

   ;; Edits
   '("c" . meow-change)
   '("d" . meow-kill)
   '("p" . meow-yank)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("y" . meow-save)

   ;; Hints
   '("0" . meow-expand-0)
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)
   
   ;; Misc
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("=" . repeat)
   '("!" . shell-command-on-region)
   '("G" . meow-grab)
   '("X" . meow-goto-line)
   '("<escape>" . meow-cancel-selection))

  (meow-normal-define-key
   '("S-<left>"  . meow-left-expand)
   '("S-<down>"  . meow-next-expand)
   '("S-<up>"    . meow-prev-expand)
   '("S-<right>" . meow-right-expand))

  (meow-leader-define-key
   '("!" . eshell)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))

  (meow-normal-define-key
   '("/" . isearch-forward-regexp)
   '("%" . anzu-isearch-query-replace-regexp))

  (defun my/meow-search-forward ()
    (interactive)
    (if (meow--direction-backward-p)
        (meow-search -1)
      (meow-search +1)))

  (defun my/meow-search-backward ()
    (interactive)
    (if (meow--direction-forward-p)
        (meow-search -1)
      (meow-search +1)))

  (meow-normal-define-key
   '("n" . my/meow-search-forward)
   '("N" . my/meow-search-backward))

  (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat)
  (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance)

  (meow-normal-define-key
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing))

  (meow-thing-register 'angle
    '(pair ("<") (">")) '(pair ("<") (">")))
 
  (setq meow-char-thing-table
   '((?\( . round)
     (?\[ . square)
     (?\{ . curly)
     (?\< . angle)
     (?\" . string)
     (?b  . buffer)
     (?f  . defun)
     (?l  . line)
     (?x  . visual-line)
     (?p  . paragraph)
     (?w  . window)
     (?.  . sentence)))

  ;; Rebind universal-argument from C-u to C-v
  (global-set-key (kbd "C-v") 'universal-argument)
  (define-key universal-argument-map (kbd "C-u") nil)
  (define-key universal-argument-map (kbd "C-v") 'universal-argument-more)

  (global-set-key (kbd "C-d") 'scroll-up-command)
  (global-set-key (kbd "C-u") 'scroll-down-command)

  (meow-global-mode 1))
