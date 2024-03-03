(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa")
(load-theme 'monotropic t)

(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;;(use-package org
;;  :config
(fset 'getlink
      (lambda (&optional arg) 
	"Keyboard macro." 
	(interactive "p") 
	(kmacro-exec-ring-item (quote ("\C-c\C-l\C-a\C-k\C-g" 0 "%d")) arg)))
(define-key org-mode-map (kbd "C-c l") #'getlink)

(global-set-key (kbd "C-c a") 'org-agenda)

(global-set-key (kbd "C-c j") 'org-journal-new-entry)
(setq org-journal-dir "d:/notes/journal")
(setq org-journal-date-format "%A, %d %B %Y")
(setq org-journal-file-format "%F")
(require 'org-journal)

(set-register ?H '(file . "d:/schedules/home-life.org"))

(require 'eval-in-repl)
(require 'python) ; if not done elsewhere
(require 'eval-in-repl-python)
(add-hook 'python-mode-hook
          '(lambda ()
	  (local-set-key (kbd "<C-return>") 'eir-eval-in-python)))

(setq-default py-shell-name "d:/envs/emacs/Scripts/python.exe")

(add-to-list 'exec-path "C:/Program Files/Git/usr/bin")

(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (add-hook 'python-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  (defun yas/org-very-safe-expand ()
    (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

  (add-hook 'org-mode-hook
            (lambda ()
              ;; yasnippet (using the new org-cycle hooks)
	      (make-variable-buffer-local 'yas/trigger-key)
	      (setq yas/trigger-key [tab])
	      (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
	      (define-key yas/keymap [tab] 'yas/next-field)))
  (yas-reload-all))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; tree sitter
;; (use-package tree-sitter
;;   :hook
;;   (python-mode . tree-sitter-hl-mode))

;; (use-package tree-sitter-langs)
;;  :after tree-sitter)
;;(require 'tree-sitter)
;;(require 'tree-sitter-langs)
;; (use-package ts-fold
;;   :load-path "~/.emacs.d/elpa/ts-fold/"
;;   :after tree-sitter)
(require 'tree-sitter)
(require 'tree-sitter-langs)